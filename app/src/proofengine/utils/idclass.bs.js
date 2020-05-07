'use strict';

var List = require("bs-platform/lib/js/list.js");
var Block = require("bs-platform/lib/js/block.js");
var Caml_exceptions = require("bs-platform/lib/js/caml_exceptions.js");
var Sml$ReasonReactExamples = require("./sml.bs.js");
var Listfuns$ReasonReactExamples = require("./listfuns.bs.js");

function catelim_string_of_idclass(a1, a2) {
  if (typeof a1 === "number") {
    switch (a1) {
      case /* NoClass */0 :
          return /* :: */[
                  "NoClass",
                  a2
                ];
      case /* FormulaClass */1 :
          return /* :: */[
                  "FormulaClass",
                  a2
                ];
      case /* VariableClass */2 :
          return /* :: */[
                  "VariableClass",
                  a2
                ];
      case /* ConstantClass */3 :
          return /* :: */[
                  "ConstantClass",
                  a2
                ];
      case /* NumberClass */4 :
          return /* :: */[
                  "NumberClass",
                  a2
                ];
      case /* StringClass */5 :
          return /* :: */[
                  "StringClass",
                  a2
                ];
      case /* OperatorClass */6 :
          return /* :: */[
                  "OperatorClass",
                  a2
                ];
      case /* SubstClass */7 :
          return /* :: */[
                  "SubstClass",
                  a2
                ];
      
    }
  } else if (a1.tag) {
    return /* :: */[
            "ListClass(",
            catelim_string_of_idclass(a1[0], /* :: */[
                  ")",
                  a2
                ])
          ];
  } else {
    return /* :: */[
            "BagClass(",
            catelim_string_of_idclass(a1[0], /* :: */[
                  ")",
                  a2
                ])
          ];
  }
}

function string_of_idclass(param) {
  return Listfuns$ReasonReactExamples.stringfn_of_catelim(catelim_string_of_idclass, param);
}

var ParseIdclass_ = Caml_exceptions.create("Idclass-ReasonReactExamples.ParseIdclass_");

function idclass_of_string(s) {
  var idwords = function (_cs, _w, _ws) {
    while(true) {
      var ws = _ws;
      var w = _w;
      var cs = _cs;
      var combine = function (w, ws) {
        if (w) {
          return /* :: */[
                  Sml$ReasonReactExamples.string_of_chars(List.rev(w)),
                  ws
                ];
        } else {
          return ws;
        }
      };
      if (cs) {
        var c = cs[0];
        if (c !== 40) {
          if (c !== 41) {
            _w = /* :: */[
              c,
              w
            ];
            _cs = cs[1];
            continue ;
          } else {
            _ws = /* :: */[
              ")",
              combine(w, ws)
            ];
            _w = /* [] */0;
            _cs = cs[1];
            continue ;
          }
        } else {
          _ws = /* :: */[
            "(",
            combine(w, ws)
          ];
          _w = /* [] */0;
          _cs = cs[1];
          continue ;
        }
      } else {
        return List.rev(combine(w, ws));
      }
    };
  };
  var idclass_of_rest = function (ws) {
    var match = List.rev(ws);
    if (match) {
      if (match[0] === ")") {
        return idclass_of_words(List.rev(match[1]));
      } else {
        throw [
              ParseIdclass_,
              s
            ];
      }
    } else {
      throw [
            ParseIdclass_,
            s
          ];
    }
  };
  var idclass_of_words = function (ws) {
    if (ws) {
      switch (ws[0]) {
        case "BagClass" :
            var match = ws[1];
            if (match) {
              if (match[0] === "(") {
                return /* BagClass */Block.__(0, [idclass_of_rest(match[1])]);
              } else {
                throw [
                      ParseIdclass_,
                      s
                    ];
              }
            } else {
              throw [
                    ParseIdclass_,
                    s
                  ];
            }
        case "ConstantClass" :
            if (ws[1]) {
              throw [
                    ParseIdclass_,
                    s
                  ];
            } else {
              return /* ConstantClass */3;
            }
        case "FormulaClass" :
            if (ws[1]) {
              throw [
                    ParseIdclass_,
                    s
                  ];
            } else {
              return /* FormulaClass */1;
            }
        case "ListClass" :
            var match$1 = ws[1];
            if (match$1) {
              if (match$1[0] === "(") {
                return /* ListClass */Block.__(1, [idclass_of_rest(match$1[1])]);
              } else {
                throw [
                      ParseIdclass_,
                      s
                    ];
              }
            } else {
              throw [
                    ParseIdclass_,
                    s
                  ];
            }
        case "NoClass" :
            if (ws[1]) {
              throw [
                    ParseIdclass_,
                    s
                  ];
            } else {
              return /* NoClass */0;
            }
        case "NumberClass" :
            if (ws[1]) {
              throw [
                    ParseIdclass_,
                    s
                  ];
            } else {
              return /* NumberClass */4;
            }
        case "OperatorClass" :
            if (ws[1]) {
              throw [
                    ParseIdclass_,
                    s
                  ];
            } else {
              return /* OperatorClass */6;
            }
        case "StringClass" :
            if (ws[1]) {
              throw [
                    ParseIdclass_,
                    s
                  ];
            } else {
              return /* StringClass */5;
            }
        case "SubstClass" :
            if (ws[1]) {
              throw [
                    ParseIdclass_,
                    s
                  ];
            } else {
              return /* SubstClass */7;
            }
        case "VariableClass" :
            if (ws[1]) {
              throw [
                    ParseIdclass_,
                    s
                  ];
            } else {
              return /* VariableClass */2;
            }
        default:
          throw [
                ParseIdclass_,
                s
              ];
      }
    } else {
      throw [
            ParseIdclass_,
            s
          ];
    }
  };
  return idclass_of_words(idwords(Sml$ReasonReactExamples.chars_of_string(s), /* [] */0, /* [] */0));
}

exports.catelim_string_of_idclass = catelim_string_of_idclass;
exports.string_of_idclass = string_of_idclass;
exports.ParseIdclass_ = ParseIdclass_;
exports.idclass_of_string = idclass_of_string;
/* No side effect */
