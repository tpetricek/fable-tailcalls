"use strict";

Object.defineProperty(exports, "__esModule", {
  value: true
});
exports.factorial = factorial;
exports.parseNum_parseTokens = parseNum_parseTokens;
exports.parseNum = parseNum;
exports.parseTokens = parseTokens;

var _fableCore = require("fable-core");

function factorial(aux, n) {
  _letrec: while (true) {
    if (n === 0) {
      return aux;
    } else {
      aux = aux * n;
      n = n - 1;
      continue _letrec;
    }
  }
}

function parseNum_parseTokens(_letrecFunction, tokens, acc, _arg1, _arg2) {
  _letrec: while (true) {
    if (_letrecFunction === "parseTokens") {
      {
        var $target1 = function $target1() {
          return _arg2.tail == null ? _fableCore.List.reverse(tokens) : function () {
            _letrecFunction = "parseTokens";
            tokens = tokens;
            _arg2 = _arg2.tail;
          }();
        };

        if (_arg2.tail != null) {
          if (_arg2.head >= "0" ? _arg2.head <= "9" : false) {
            var x = _arg2.head;
            var xs = _arg2.tail;
            {
              _letrecFunction = "parseNum";
              tokens = tokens;
              acc = _fableCore.List.ofArray([x]);
              _arg1 = xs;
              continue _letrec;
            }
          } else {
            $target1();
            continue _letrec;
          }
        } else {
          $target1();
          continue _letrec;
        }
      }
    } else {
      if (_letrecFunction === "parseNum") {
        {
          var $target1 = function $target1() {
            _letrecFunction = "parseTokens";
            tokens = _fableCore.List.ofArray([_fableCore.List.reverse(acc)], tokens);
            _arg2 = _arg1;
          };

          if (_arg1.tail != null) {
            if (_arg1.head >= "0" ? _arg1.head <= "9" : false) {
              var x = _arg1.head;
              var xs = _arg1.tail;
              {
                _letrecFunction = "parseNum";
                tokens = tokens;
                acc = _fableCore.List.ofArray([x], acc);
                _arg1 = xs;
                continue _letrec;
              }
            } else {
              $target1();
              continue _letrec;
            }
          } else {
            $target1();
            continue _letrec;
          }
        }
      }
    }
  }
}

function parseNum(tokens, acc, _arg1) {
  return parseNum_parseTokens("parseNum", tokens, acc, _arg1);
}

function parseTokens(tokens, _arg2) {
  return parseNum_parseTokens("parseTokens", tokens, null, null, _arg2);
}