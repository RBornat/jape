'use strict';

var Curry = require("bs-platform/lib/js/curry.js");
var Hashtbl = require("bs-platform/lib/js/hashtbl.js");
var Caml_builtin_exceptions = require("bs-platform/lib/js/caml_builtin_exceptions.js");

function F(AAA) {
  var store = Hashtbl.create(undefined, AAA.size);
  var lookup = function (k) {
    try {
      return Hashtbl.find(store, k);
    }
    catch (exn){
      if (exn === Caml_builtin_exceptions.not_found) {
        var v = Curry._1(AAA.$$eval, k);
        Hashtbl.add(store, k, v);
        return v;
      } else {
        throw exn;
      }
    }
  };
  var reset = function (param) {
    return Hashtbl.clear(store);
  };
  return {
          lookup: lookup,
          reset: reset
        };
}

exports.F = F;
/* No side effect */
