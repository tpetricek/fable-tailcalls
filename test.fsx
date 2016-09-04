module Test2

let rec factorial aux n =
  if n = 0 then aux
  else factorial (aux * n) (n - 1)

let rec parseNum tokens acc = function
  | x::xs when x >= '0' && x <= '9' ->
      parseNum tokens (x::acc) xs 
  | xs -> parseTokens ((List.rev acc)::tokens) xs

and parseTokens tokens = function
  | x::xs when x >= '0' && x <= '9' ->
      parseNum tokens [x] xs
  | x::xs -> parseTokens tokens xs
  | [] -> List.rev tokens    
