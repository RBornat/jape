'use strict';

var List = require("bs-platform/lib/js/list.js");
var Curry = require("bs-platform/lib/js/curry.js");
var Caml_obj = require("bs-platform/lib/js/caml_obj.js");
var Pervasives = require("bs-platform/lib/js/pervasives.js");
var Caml_option = require("bs-platform/lib/js/caml_option.js");
var Sml$ReasonReactExamples = require("./sml.bs.js");
var Listfuns$ReasonReactExamples = require("./listfuns.bs.js");
var Optionfuns$ReasonReactExamples = require("./optionfuns.bs.js");
var Miscellaneous$ReasonReactExamples = require("../miscellaneous.bs.js");

function isempty(xs) {
  return xs === /* [] */0;
}

function $pipe$neg$great(a, b) {
  return /* :: */[
          /* tuple */[
            a,
            b
          ],
          /* [] */0
        ];
}

function $plus$plus(m, n) {
  return Pervasives.$at(n, m);
}

function mapped(same, mapping, a) {
  var _param = mapping;
  while(true) {
    var param = _param;
    if (param) {
      var match = param[0];
      if (Curry._1(same, /* tuple */[
              match[0],
              a
            ])) {
        return Caml_option.some(match[1]);
      } else {
        _param = param[1];
        continue ;
      }
    } else {
      return ;
    }
  };
}

function $neg$neg(_xs, _ys) {
  while(true) {
    var ys = _ys;
    var xs = _xs;
    if (xs) {
      var ps = xs[1];
      var match = xs[0];
      var x = match[0];
      if (Listfuns$ReasonReactExamples.member(/* tuple */[
              x,
              ys
            ])) {
        _ys = Listfuns$ReasonReactExamples.listsub((function (param) {
                return Caml_obj.caml_equal(param[0], param[1]);
              }), ys, /* :: */[
              x,
              /* [] */0
            ]);
        _xs = ps;
        continue ;
      } else {
        return /* :: */[
                /* tuple */[
                  x,
                  match[1]
                ],
                $neg$neg(ps, ys)
              ];
      }
    } else {
      return /* [] */0;
    }
  };
}

function $less$at$great(mapping, a) {
  return mapped((function (param) {
                return Caml_obj.caml_equal(param[0], param[1]);
              }), mapping, a);
}

function mem(a1, _a2) {
  while(true) {
    var a2 = _a2;
    if (a2) {
      if (Caml_obj.caml_equal(a1, a2[0])) {
        return true;
      } else {
        _a2 = a2[1];
        continue ;
      }
    } else {
      return false;
    }
  };
}

function lfold(f, r, m) {
  var _a1 = /* [] */0;
  var _a2 = r;
  var _a3 = m;
  while(true) {
    var a3 = _a3;
    var a2 = _a2;
    var a1 = _a1;
    if (a3) {
      var map = a3[1];
      var pair = a3[0];
      var x = pair[0];
      if (mem(x, a1)) {
        _a3 = map;
        continue ;
      } else {
        _a3 = map;
        _a2 = Curry._1(f, /* tuple */[
              pair,
              a2
            ]);
        _a1 = /* :: */[
          x,
          a1
        ];
        continue ;
      }
    } else {
      return a2;
    }
  };
}

function remapping(param) {
  var f = param[0];
  return lfold((function (param) {
                var m = Miscellaneous$ReasonReactExamples.uncurry2($pipe$neg$great, Curry._1(f, param[0]));
                return Pervasives.$at(param[1], m);
              }), /* [] */0, param[1]);
}

function aslist(m) {
  return Listfuns$ReasonReactExamples.seteq((function (param, param$1) {
                return Caml_obj.caml_equal(param[0], param$1[0]);
              }), m);
}

function fromlist(m) {
  return m;
}

function dom(m) {
  return List.map((function (param) {
                return param[0];
              }), aslist(m));
}

function ran(m) {
  return List.map((function (param) {
                return param[1];
              }), aslist(m));
}

function rawaslist(m) {
  return m;
}

function rawdom(m) {
  return List.map((function (param) {
                return param[0];
              }), m);
}

function rawran(m) {
  return List.map((function (param) {
                return param[1];
              }), m);
}

function formappingpairs(param) {
  var mapping = param[1];
  var f = param[0];
  return List.iter((function (d) {
                return Curry._1(f, /* tuple */[
                            d,
                            Optionfuns$ReasonReactExamples._The($less$at$great(mapping, d))
                          ]);
              }), dom(mapping));
}

function mkmap(pairs) {
  return Sml$ReasonReactExamples.nj_fold((function (param) {
                var match = param[0];
                var n = $pipe$neg$great(match[0], match[1]);
                return Pervasives.$at(n, param[1]);
              }), pairs, /* [] */0);
}

function catelim_string_of_mapping(astring, bstring, sep, mapping, ss) {
  return /* :: */[
          "<<",
          Listfuns$ReasonReactExamples.catelim_string_of_list((function (param, ss) {
                    return /* :: */[
                            "(",
                            Curry._2(astring, param[0], /* :: */[
                                  "|->",
                                  Curry._2(bstring, param[1], /* :: */[
                                        ")",
                                        ss
                                      ])
                                ])
                          ];
                  }), sep)(List.rev(mapping), /* :: */[
                ">>",
                ss
              ])
        ];
}

function string_of_mapping(a, b) {
  return (function (param) {
      return Listfuns$ReasonReactExamples.stringfn_of_catelim((function (param, param$1) {
                    return catelim_string_of_mapping((function (param, param$1) {
                                  return Listfuns$ReasonReactExamples.catelim_of_stringfn(a, param, param$1);
                                }), (function (param, param$1) {
                                  return Listfuns$ReasonReactExamples.catelim_of_stringfn(b, param, param$1);
                                }), "++", param, param$1);
                  }), param);
    });
}

var empty = /* [] */0;

exports.empty = empty;
exports.isempty = isempty;
exports.$pipe$neg$great = $pipe$neg$great;
exports.$plus$plus = $plus$plus;
exports.mapped = mapped;
exports.$neg$neg = $neg$neg;
exports.$less$at$great = $less$at$great;
exports.mem = mem;
exports.lfold = lfold;
exports.remapping = remapping;
exports.aslist = aslist;
exports.fromlist = fromlist;
exports.dom = dom;
exports.ran = ran;
exports.rawaslist = rawaslist;
exports.rawdom = rawdom;
exports.rawran = rawran;
exports.formappingpairs = formappingpairs;
exports.mkmap = mkmap;
exports.catelim_string_of_mapping = catelim_string_of_mapping;
exports.string_of_mapping = string_of_mapping;
/* No side effect */
