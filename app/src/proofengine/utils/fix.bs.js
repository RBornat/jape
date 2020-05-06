'use strict';

var Curry = require("bs-platform/lib/js/curry.js");
var Hashtbl = require("bs-platform/lib/js/hashtbl.js");
var Caml_builtin_exceptions = require("bs-platform/lib/js/caml_builtin_exceptions.js");

function fix(ff) {
  return Curry._1(ff, fix(ff));
}

function memofix(mem, ff) {
  var proxy = function (k) {
    try {
      return Hashtbl.find(mem, k);
    }
    catch (exn){
      if (exn === Caml_builtin_exceptions.not_found) {
        var v = Curry._2(ff, proxy, k);
        Hashtbl.add(mem, k, v);
        return v;
      } else {
        throw exn;
      }
    }
  };
  return proxy;
}

exports.fix = fix;
exports.memofix = memofix;
/* No side effect */
