"use strict";

Object.defineProperty(exports, "__esModule", {
  value: true
});
exports.sum = sum;
exports.factorial = factorial;
exports.parseNum_parseTokens = parseNum_parseTokens;
exports.parseNum = parseNum;
exports.parseTokens = parseTokens;

var _fableCore = require("fable-core");

function sum(v, m, s) {
  _letrec: while (true) {
    if (v >= m) {
      return s;
    } else {
      var $tmp0 = v + 1;
      var $tmp1 = m;
      var $tmp2 = s + v;
      v = $tmp0;
      m = $tmp1;
      s = $tmp2;
      continue _letrec;
    }
  }
}

_fableCore.String.fsFormat("sum 0 10000L 0 -> %d")(function (x) {
  console.log(x);
})(sum(0, 10000, 0));

_fableCore.String.fsFormat("(must be equal to 49995000)")(function (x) {
  console.log(x);
});

function factorial(aux, n) {
  _letrec: while (true) {
    if (n === 0) {
      return aux;
    } else {
      var $tmp0 = aux * n;
      var $tmp1 = n - 1;
      aux = $tmp0;
      n = $tmp1;
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
            var $tmp0 = tokens;
            var $tmp1 = _arg2.tail;
            tokens = $tmp0;
            _arg2 = $tmp1;
          }();
        };

        if (_arg2.tail != null) {
          if (_arg2.head >= "0" ? _arg2.head <= "9" : false) {
            var x = _arg2.head;
            var xs = _arg2.tail;
            {
              _letrecFunction = "parseNum";
              var $tmp0 = tokens;

              var $tmp1 = _fableCore.List.ofArray([x]);

              var $tmp2 = xs;
              tokens = $tmp0;
              acc = $tmp1;
              _arg1 = $tmp2;
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
          var _$target = function _$target() {
            _letrecFunction = "parseTokens";

            var $tmp0 = _fableCore.List.ofArray([_fableCore.List.reverse(acc)], tokens);

            var $tmp1 = _arg1;
            tokens = $tmp0;
            _arg2 = $tmp1;
          };

          if (_arg1.tail != null) {
            if (_arg1.head >= "0" ? _arg1.head <= "9" : false) {
              var _x = _arg1.head;
              var _xs = _arg1.tail;
              {
                _letrecFunction = "parseNum";
                var _$tmp = tokens;

                var _$tmp2 = _fableCore.List.ofArray([_x], acc);

                var _$tmp3 = _xs;
                tokens = _$tmp;
                acc = _$tmp2;
                _arg1 = _$tmp3;
                continue _letrec;
              }
            } else {
              _$target();

              continue _letrec;
            }
          } else {
            _$target();

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