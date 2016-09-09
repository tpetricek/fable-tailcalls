#if INTERACTIVE
#I "../Fable/build/fable/bin"
#r "Fable.Core.dll"
#r "Fable.Compiler.dll"
#r "Fable.Client.Node.exe"
#r "FSharp.Compiler.Service.dll"
#else
module Fable.Plugins.Tailcalls
#endif

open Fable
open Fable.AST
open Fable.AST.Fable
open System.Collections.Generic

// --------------------------------------------------------------------------------------
// Manipulating Fable AST
// --------------------------------------------------------------------------------------

let rec visitDeclaration f d = 
  match d with
  | ActionDeclaration(e, a) -> ActionDeclaration(visitExpr f e, a)
  | EntityDeclaration(e, n, d, s) -> EntityDeclaration(e, n, List.map (visitDeclaration f) d, s)
  | MemberDeclaration(m, n, a, b, s) -> MemberDeclaration(m, n, a, visitExpr f b, s)

and visitExpr (f:Expr -> Choice<unit, Expr, Expr>) e = 
  match f e with
  | Choice1Of3 () -> e
  | Choice2Of3 e -> e
  | Choice3Of3 e ->
  match e with
  | Value(Spread e) -> Value(Spread(visitExpr f e))
  | Value(TupleConst es) -> Value(TupleConst(List.map (visitExpr f) es))
  | Value(Lambda(args,e)) -> Value(Lambda(args, visitExpr f e))
  | ObjExpr(d, i, b, r) -> ObjExpr(List.map (visitDeclaration f) d, i, Option.map (visitExpr f) b, r)
  | IfThenElse(e1, e2, e3, r) -> IfThenElse(visitExpr f e1, visitExpr f e2, visitExpr f e3, r)
  | Apply(e1, es, k, t, r) -> Apply(visitExpr f e1, List.map (visitExpr f) es, k, t, r)
  | Quote(e) -> Quote(visitExpr f e)
  | Throw(e, t, r) -> Throw(visitExpr f e, t, r)
  | DebugBreak r -> DebugBreak r
  | Loop(While(e1, e2), r) -> Loop(While(visitExpr f e1, visitExpr f e2), r)
  | Loop(For(i, e1, e2, e3, u), r) -> Loop(For(i, visitExpr f e1, visitExpr f e2, visitExpr f e3, u), r)
  | Loop(ForOf(i, e1, e2), r) -> Loop(ForOf(i, visitExpr f e1, visitExpr f e2), r)
  | VarDeclaration(v, e, m) -> VarDeclaration(v, visitExpr f e, m)
  | Set(e1, p, e2, r) -> Set(visitExpr f e1, Option.map (visitExpr f) p, visitExpr f e2, r)
  | Sequential(es, r) -> Sequential(List.map (visitExpr f) es, r)
  | TryCatch(e, c, n, r) -> TryCatch(visitExpr f e, Option.map (fun (i, e) -> i, visitExpr f e) c, Option.map (visitExpr f) n, r)
  | Wrapped(e, t) -> Wrapped(visitExpr f e, t)
  | Label(l, e, r) -> Label(l, visitExpr f e, r)
  | Return(e, r) -> Return(visitExpr f e, r)
  | Break _ | Continue _ | Value _ -> e
  
let foldDeclaration s f d = 
  let mutable state = s
  visitDeclaration (fun e -> state <- f state e; Choice3Of3 e) d |> ignore
  state

let rec mapDeclaration f d = 
  visitDeclaration (f >> Choice3Of3) d

and mapExpr f e = 
  visitExpr (f >> Choice3Of3) e

// --------------------------------------------------------------------------------------
// Detecting recursive function groups
// --------------------------------------------------------------------------------------

type Context = 
  { File : File
    TopLevel : IDictionary<string, Ident list * Expr * SourceLocation> }

let calculateRecursiveGroups ctx =
  /// Iterate over declarations and find top-level functions that each function calls
  let _, calls = ctx.File.Declarations |> List.fold (fun (known, res) decl ->
    match decl with
    | MemberDeclaration(m, _, _, _, _) as decl ->
      let calls =   
        decl |> foldDeclaration [] (fun calls e ->
          match e with
          | Apply(Value(IdentValue(id)), _, _, _, _) when ctx.TopLevel.ContainsKey id.name ->
              id.name :: calls
          | Apply(Value(TypeRef e), [Value(StringConst n)], _, _, _) 
              when ctx.TopLevel.ContainsKey n && e.File = ctx.File.Root.File && e.FullName = ctx.File.Root.FullName 
                -> n::calls
          | _ -> calls )
      let calls = calls |> List.filter (fun n -> not (Set.contains n known))
      Set.add m.Name known, (m.Name, calls)::res
    | _ -> known, res) (Set.empty, [])

  /// Compute recursive groups (by walking back and adding functions 
  /// until the set of functions that the group calls is empty)
  let rec collectRecursiveGroups acc group groupCalls (calls:list<string * string list>) = 
    match calls with
    | (n, deps)::calls when not (Set.isEmpty groupCalls) ->
        let groupCalls = Set.remove n (Set.union (set deps) groupCalls)
        if Set.isEmpty groupCalls then
          collectRecursiveGroups ((n::group)::acc) [] Set.empty calls
        else
          collectRecursiveGroups acc (n::group) groupCalls calls  
    | (n, [])::calls -> collectRecursiveGroups acc [] Set.empty calls
    | (n1, [n2])::calls when n1 = n2 -> collectRecursiveGroups ([n1]::acc) [] Set.empty calls
    | (n, deps)::calls -> collectRecursiveGroups acc [n] (set deps) calls
    | [] -> List.rev (group::acc)

  /// Now we have mutually recursive groups in the file
  collectRecursiveGroups [] [] Set.empty (List.rev calls)
  |> List.map List.rev


// --------------------------------------------------------------------------------------
// Generating loops from tail-recursive calls
// --------------------------------------------------------------------------------------

let hasTail = function
  | [] | [_] -> false
  | _ -> true 

let generateGroupFunction ctx (group:string list) =
  let id n = { name=n; typ=Type.Any }
  let funcId = { name="_letrecFunction"; typ=Type.String }
  
  // Generates 'call', i.e. assigns parameters and then uses 'continue'
  // When `insertContinue' is false, we are inside a $target function and
  // continue is added after the call to the function (in pattern matching)
  let makeTailCall insertContinue name args = 
    let vars, _, _ = ctx.TopLevel.[name]
    let assigns =
      [0..vars.Length-1]
      |> List.map (fun i -> "$tmp" + (string i) |> Fable.Util.makeIdent)
      |> List.zip3 vars args
    Expr.Sequential
      ( [ if hasTail group then
            yield Set(Value(IdentValue funcId), None, Value(StringConst name), None)
          for _,expr,tempVar in assigns do
            yield VarDeclaration(tempVar, expr, true)
          for var,_,tempVar in assigns do
            yield Set(Value(IdentValue var), None, Value(IdentValue tempVar), None)
          if insertContinue then
            yield Expr.Continue(Some(id "_letrec"), None) ], None)

  let rec replaceTailCalls insertContinue body = 
    body |> visitExpr (function
      // Declaration of match '$target' function - rather than injecting
      // 'continue' inside lambda, we inject continue _after_ the call to $target
      | VarDeclaration(id, e, mut) when id.name.StartsWith("$target") ->
          let res = VarDeclaration(id, replaceTailCalls false e, mut)
          Choice2Of3(res)

      // Inject 'continue' call after the call to $target (which sets variables)
      | Apply(Value(IdentValue(target)), [], k, t, r) as targetCall when target.name.StartsWith("$target") ->
          let continueExpr = Expr.Continue(Some(id "_letrec"), None)
          Choice2Of3(Sequential([targetCall; continueExpr], None))

      // Ordinary calls - replace call with a tail-call via contineu          
      | Apply(Value(IdentValue(id)), args, _, _, _) when ctx.TopLevel.ContainsKey id.name ->
          Choice3Of3(makeTailCall insertContinue id.name args)
      | Apply(Apply(Value(TypeRef e), [Value(StringConst n)], _, _, _), args, _, _, _) 
        when ctx.TopLevel.ContainsKey n && e.File = ctx.File.Root.File && e.FullName = ctx.File.Root.FullName ->
          Choice3Of3(makeTailCall insertContinue n args)

      | e -> Choice3Of3 e)

  // Generate condition that runs appropriate body depending
  // on the vale of the "_letrecFunction" parameter
  let body = 
    let loopBody = 
      match group with 
      | [n] ->
          let args, e, loc = ctx.TopLevel.[n]
          Return(replaceTailCalls true e, None)
      | group ->
          group |> List.fold (fun body n ->
            let args, e, loc = ctx.TopLevel.[n]
            IfThenElse
              ( Apply
                  ( Value(BinaryOp BinaryEqualStrict),
                    [ Value(IdentValue funcId); Value(StringConst n) ],
                    ApplyMeth, Boolean, None ),
                Return(replaceTailCalls true e, None), body,
                None )) (Wrapped(Value Null, Unit))
    Label(
      id "_letrec", 
      Loop(While(Value(BoolConst true), loopBody), None), None)

  // Calculate aggregated source location
  let loc = 
    group 
    |> List.map (fun n -> let _, _, l = ctx.TopLevel.[n] in l) 
    |> function [] -> SourceLocation.Empty | xs -> List.reduce (+) xs

  // Generate new function that takes union of the arguments
  let name = group |> String.concat "_"
  let args = 
    group 
    |> List.collect (fun g -> let a, _, _ = ctx.TopLevel.[g] in a)
    |> List.groupBy (fun i -> i.name)
    |> List.map (function n, [g] -> g | n, _ -> { name=n; typ=Type.Any })
  let appendMulti e l = if not(hasTail group) then l else e::l
  name, args, 
    MemberDeclaration
      ( Member(name, Method, appendMulti String [for a in args -> a.typ], Type.Any), 
        None, appendMulti funcId args, body, loc )

// --------------------------------------------------------------------------------------
// Generating loops from tail-recursive calls
// --------------------------------------------------------------------------------------

let generateCall ctx fname fgroupname fargs actualArgs typ = 
  // The merged function takes union of arguments of all the functions in the group
  // We pass values to all the arguments of this function and 'null' to all others
  let actualLookup = 
    Map.ofList [ for a in actualArgs -> a.name, Value(IdentValue a) ]
  let args = 
    [ for a in fargs ->
        match actualLookup.TryFind a.name with
        | Some a -> a
        | None -> Value(Null) ]
  let funcId = Value(StringConst fname)
  Apply(Value(IdentValue({ name=fgroupname; typ=Type.Any })), funcId::args, ApplyMeth, typ, None)


let transform file =   
  // Collect names of top-level declarations in the file
  let ctx = 
    { File = file
      TopLevel = file.Declarations |> List.choose (function 
        MemberDeclaration(n, _, a, b, l) as m -> Some(n.Name, (a, b, l)) | _ -> None) |> dict }

  // Get recursive groups, generate functions for them
  let groups = calculateRecursiveGroups ctx
  let funcs = groups |> List.map (fun g -> g, generateGroupFunction ctx g)
  let declLookup = Map.ofList [ for g, (_, _, f) in funcs -> defaultArg (List.tryHead g) "", f ]
  let replaceLookup = Map.ofList [ for g, (nn, a, _) in funcs do for n in g -> n, (nn, a) ]
  let singleRecFunc = groups |> List.choose (function [s] -> Some s | _ -> None) |> set

  // Inject newly generated functions & replce functions in group with calls to new function
  let decls = 
    file.Declarations |> List.collect (function
      | MemberDeclaration(m, n, a, b, s) when singleRecFunc.Contains m.Name -> 
          [ declLookup.[m.Name] ] 
      | MemberDeclaration(m, n, a, b, s) ->   
          [ match declLookup.TryFind m.Name with
            | Some f -> yield f
            | _ -> ()
            match replaceLookup.TryFind m.Name with
            | Some(fn, fargs) -> 
                let body = generateCall ctx m.Name fn fargs a m.ReturnType 
                yield MemberDeclaration(m, n, a, body, s) 
            | _ -> yield MemberDeclaration(m, n, a, b, s) ]
      | d -> [d])
  File(file.FileName, file.Root, decls, file.UsedVarNames)


/// Expose as a Fable plugin
type TailcallRewrite() =
  interface IRewritePlugin with
    member x.Rewrite(files) = files |> Seq.map transform 

#if INTERACTIVE
System.Environment.CurrentDirectory <- __SOURCE_DIRECTORY__
let succ, proj = Fable.Client.Node.Main.parseFable [| "--projFile"; "test.fsx"; "--copyExt"; "true" |]
let files = proj.Extra.["<fable>"] :?> seq<Fable.File>
let file = files |> Seq.head
#endif