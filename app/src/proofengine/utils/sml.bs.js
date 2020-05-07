'use strict';

var List = require("bs-platform/lib/js/list.js");
var Curry = require("bs-platform/lib/js/curry.js");
var $$String = require("bs-platform/lib/js/string.js");
var Caml_string = require("bs-platform/lib/js/caml_string.js");
var Caml_exceptions = require("bs-platform/lib/js/caml_exceptions.js");

function $less$dot$great(f, g, x) {
  return Curry._1(f, Curry._1(g, x));
}

function nj_fold(f, xs, z) {
  if (xs) {
    return Curry._1(f, /* tuple */[
                xs[0],
                nj_fold(f, xs[1], z)
              ]);
  } else {
    return z;
  }
}

function nj_revfold(f, _xs, _z) {
  while(true) {
    var z = _z;
    var xs = _xs;
    if (xs) {
      _z = Curry._1(f, /* tuple */[
            xs[0],
            z
          ]);
      _xs = xs[1];
      continue ;
    } else {
      return z;
    }
  };
}

function chars_of_string(s) {
  var len = s.length;
  var e = function (n) {
    if (n === len) {
      return /* [] */0;
    } else {
      return /* :: */[
              Caml_string.get(s, n),
              e(n + 1 | 0)
            ];
    }
  };
  return e(0);
}

function explode(param) {
  var param$1 = chars_of_string(param);
  return List.map((function (param) {
                return $$String.make(1, param);
              }), param$1);
}

function implode(param) {
  return $$String.concat("", param);
}

function string_of_chars(param) {
  var param$1 = List.map((function (param) {
          return $$String.make(1, param);
        }), param);
  return $$String.concat("", param$1);
}

function fst_of_3(param) {
  return param[0];
}

function snd_of_3(param) {
  return param[1];
}

function thrd(param) {
  return param[2];
}

function fst_of_6(param) {
  return param[0];
}

function fst_of_7(param) {
  return param[0];
}

function $$null(xs) {
  return xs === /* [] */0;
}

var OrdOf_ = Caml_exceptions.create("Sml-ReasonReactExamples.OrdOf_");

function ordof(s, i) {
  try {
    return Caml_string.get(s, i);
  }
  catch (exn){
    throw [
          OrdOf_,
          s,
          i
        ];
  }
}

function ord(s) {
  return ordof(s, 0);
}

function revapp(f, xs) {
  if (xs) {
    revapp(f, xs[1]);
    return Curry._1(f, xs[0]);
  } else {
    return /* () */0;
  }
}

exports.$less$dot$great = $less$dot$great;
exports.nj_fold = nj_fold;
exports.nj_revfold = nj_revfold;
exports.chars_of_string = chars_of_string;
exports.explode = explode;
exports.implode = implode;
exports.string_of_chars = string_of_chars;
exports.fst_of_3 = fst_of_3;
exports.snd_of_3 = snd_of_3;
exports.thrd = thrd;
exports.fst_of_6 = fst_of_6;
exports.fst_of_7 = fst_of_7;
exports.$$null = $$null;
exports.OrdOf_ = OrdOf_;
exports.ordof = ordof;
exports.ord = ord;
exports.revapp = revapp;
/* No side effect */
