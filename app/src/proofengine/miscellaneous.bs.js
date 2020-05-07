'use strict';

var List = require("bs-platform/lib/js/list.js");
var Curry = require("bs-platform/lib/js/curry.js");
var Belt_Int = require("bs-platform/lib/js/belt_Int.js");
var Pervasives = require("bs-platform/lib/js/pervasives.js");
var Caml_exceptions = require("bs-platform/lib/js/caml_exceptions.js");

function iter(f, param) {
  for(var i = param[0] ,i_finish = param[1]; i <= i_finish; ++i){
    Curry._1(f, i);
  }
  return /* () */0;
}

var AtoI_ = Caml_exceptions.create("Miscellaneous-ReasonReactExamples.AtoI_");

function atoi(s) {
  var match = Belt_Int.fromString(s);
  if (match !== undefined) {
    return match;
  } else {
    throw AtoI_;
  }
}

function sum(ns) {
  return List.fold_left((function (prim, prim$1) {
                return prim + prim$1 | 0;
              }), 0, ns);
}

function curry2(f, a, b) {
  return Curry._1(f, /* tuple */[
              a,
              b
            ]);
}

function uncurry2(f, param) {
  return Curry._2(f, param[0], param[1]);
}

function curry3(f, a, b, c) {
  return Curry._1(f, /* tuple */[
              a,
              b,
              c
            ]);
}

function uncurry3(f, param) {
  return Curry._3(f, param[0], param[1], param[2]);
}

function swapargs(f, a, b) {
  return Curry._2(f, b, a);
}

function $less$question$great(f, a, b) {
  return Curry._2(f, b, a);
}

function string_of_ref(f, param) {
  var a = param.contents;
  return "ref(" + Curry._1(f, a) + ")";
}

function earlierpair(lta, ltb, param, param$1) {
  var a$prime = param$1[0];
  var a = param[0];
  if (Curry._2(lta, a, a$prime)) {
    return true;
  } else if (Curry._2(lta, a$prime, a)) {
    return false;
  } else {
    return Curry._2(ltb, param[1], param$1[1]);
  }
}

var Catastrophe_ = Caml_exceptions.create("Miscellaneous-ReasonReactExamples.Catastrophe_");

var ParseError_ = Caml_exceptions.create("Miscellaneous-ReasonReactExamples.ParseError_");

var Tacastrophe_ = Caml_exceptions.create("Miscellaneous-ReasonReactExamples.Tacastrophe_");

var utf8BOM = "\xef\xbb\xbf";

var errstream = {
  contents: Pervasives.stderr
};

var reporteropen = {
  contents: false
};

function create_reportfile(s) {
  if (reporteropen.contents) {
    Pervasives.close_out(errstream.contents);
  }
  errstream.contents = Pervasives.open_out(s);
  Pervasives.output_string(errstream.contents, utf8BOM);
  reporteropen.contents = true;
  return /* () */0;
}

function close_reportfile(param) {
  if (reporteropen.contents) {
    Pervasives.close_out(errstream.contents);
  }
  errstream.contents = Pervasives.stderr;
  reporteropen.contents = false;
  return /* () */0;
}

function consolereport(strings) {
  var e = errstream.contents;
  List.iter((function (param) {
          return Pervasives.output_string(e, param);
        }), strings);
  Pervasives.output_string(e, "\n");
  return Pervasives.flush(e);
}

function consolequery(param) {
  var no = param[2];
  var yes = param[1];
  List.iter((function (param) {
          return Pervasives.output_string(Pervasives.stdout, param);
        }), param[0]);
  Pervasives.output_string(Pervasives.stdout, "  ");
  var q = function (param) {
    List.iter((function (param) {
            return Pervasives.output_string(Pervasives.stdout, param);
          }), /* :: */[
          yes,
          /* :: */[
            "(y)/",
            /* :: */[
              no,
              /* :: */[
                "(n)? ",
                /* [] */0
              ]
            ]
          ]
        ]);
    Pervasives.flush(Pervasives.stdout);
    var match = Pervasives.input_char(Pervasives.stdin);
    if (match >= 90) {
      if (match !== 110) {
        if (match !== 121) {
          return s(/* () */0);
        } else {
          return true;
        }
      } else {
        return false;
      }
    } else if (match !== 78) {
      if (match >= 89) {
        return true;
      } else {
        return s(/* () */0);
      }
    } else {
      return false;
    }
  };
  var s = function (_param) {
    while(true) {
      var match = Pervasives.input_char(Pervasives.stdin);
      if (match !== 10) {
        _param = /* () */0;
        continue ;
      } else {
        Pervasives.output_string(Pervasives.stdout, "Pardon? ");
        return q(/* () */0);
      }
    };
  };
  return q(/* () */0);
}

var Error_ = Caml_exceptions.create("Miscellaneous-ReasonReactExamples.Error_");

function error(strings) {
  consolereport(strings);
  throw Error_;
}

var applyconjectures = {
  contents: "none"
};

var autoAdditiveLeft = {
  contents: false
};

var autoAdditiveRight = {
  contents: false
};

var autoselect = {
  contents: true
};

var givenMenuTactic = {
  contents: ""
};

var multiassumptionlines = {
  contents: true
};

var foldformulae = {
  contents: false
};

var foldsequents = {
  contents: false
};

var lemmacount = {
  contents: 0
};

var multihypsel = {
  contents: false
};

var resolvepossible = {
  contents: false
};

var screenpositiondebug = {
  contents: true
};

var seektipselection = {
  contents: true
};

var selectiondebug = {
  contents: true
};

var textselectionmode = {
  contents: "subformula"
};

var truncatereasons = {
  contents: false
};

var tryresolution = {
  contents: true
};

var disproofdebug = {
  contents: false
};

exports.iter = iter;
exports.AtoI_ = AtoI_;
exports.atoi = atoi;
exports.sum = sum;
exports.curry2 = curry2;
exports.uncurry2 = uncurry2;
exports.curry3 = curry3;
exports.uncurry3 = uncurry3;
exports.swapargs = swapargs;
exports.$less$question$great = $less$question$great;
exports.string_of_ref = string_of_ref;
exports.earlierpair = earlierpair;
exports.Catastrophe_ = Catastrophe_;
exports.ParseError_ = ParseError_;
exports.Tacastrophe_ = Tacastrophe_;
exports.utf8BOM = utf8BOM;
exports.errstream = errstream;
exports.reporteropen = reporteropen;
exports.create_reportfile = create_reportfile;
exports.close_reportfile = close_reportfile;
exports.consolereport = consolereport;
exports.consolequery = consolequery;
exports.Error_ = Error_;
exports.error = error;
exports.applyconjectures = applyconjectures;
exports.autoAdditiveLeft = autoAdditiveLeft;
exports.autoAdditiveRight = autoAdditiveRight;
exports.autoselect = autoselect;
exports.givenMenuTactic = givenMenuTactic;
exports.multiassumptionlines = multiassumptionlines;
exports.foldformulae = foldformulae;
exports.foldsequents = foldsequents;
exports.lemmacount = lemmacount;
exports.multihypsel = multihypsel;
exports.resolvepossible = resolvepossible;
exports.screenpositiondebug = screenpositiondebug;
exports.seektipselection = seektipselection;
exports.selectiondebug = selectiondebug;
exports.textselectionmode = textselectionmode;
exports.truncatereasons = truncatereasons;
exports.tryresolution = tryresolution;
exports.disproofdebug = disproofdebug;
/* No side effect */
