'use strict';

var Curry = require("bs-platform/lib/js/curry.js");
var Caml_option = require("bs-platform/lib/js/caml_option.js");
var Caml_exceptions = require("bs-platform/lib/js/caml_exceptions.js");
var Sml$ReasonReactExamples = require("./sml.bs.js");
var Miscellaneous$ReasonReactExamples = require("../miscellaneous.bs.js");

var None_ = Caml_exceptions.create("Optionfuns-ReasonReactExamples.None_");

function _The(param) {
  if (param !== undefined) {
    return Caml_option.valFromOption(param);
  } else {
    throw None_;
  }
}

function _Some(x) {
  return Caml_option.some(x);
}

function bool_of_opt(param) {
  return param !== undefined;
}

function optf(f, param) {
  if (param !== undefined) {
    return Caml_option.some(Curry._1(f, Caml_option.valFromOption(param)));
  }
  
}

function somef(f, x) {
  var v = Curry._1(f, x);
  if (v !== undefined) {
    return v;
  } else {
    return Caml_option.some(x);
  }
}

function anyway(f, x) {
  var match = Curry._1(f, x);
  if (match !== undefined) {
    return Caml_option.valFromOption(match);
  } else {
    return x;
  }
}

function failpt(f, _x) {
  while(true) {
    var x = _x;
    var match = Curry._1(f, x);
    if (match !== undefined) {
      _x = Caml_option.valFromOption(match);
      continue ;
    } else {
      return x;
    }
  };
}

function $amp$tilde$tilde(v, g) {
  if (v !== undefined) {
    return Curry._1(g, Caml_option.valFromOption(v));
  }
  
}

function $pipe$tilde$tilde(v, g) {
  if (v !== undefined) {
    return v;
  } else {
    return Curry._1(g, /* () */0);
  }
}

function $amp$tilde(f, g, x) {
  return $amp$tilde$tilde(Curry._1(f, x), g);
}

function $pipe$tilde(f, g, x) {
  var v = Curry._1(f, x);
  if (v !== undefined) {
    return v;
  } else {
    return Curry._1(g, x);
  }
}

function optioncompose(param, x) {
  var match = Curry._1(param[1], x);
  if (match !== undefined) {
    return Caml_option.some(Curry._1(param[0], Caml_option.valFromOption(match)));
  }
  
}

function optionmap(f, param) {
  if (param) {
    var xs = param[1];
    return $amp$tilde$tilde(Curry._1(f, param[0]), (function (x) {
                  return $amp$tilde$tilde(optionmap(f, xs), (function (xs) {
                                return /* :: */[
                                        x,
                                        xs
                                      ];
                              }));
                }));
  } else {
    return /* [] */0;
  }
}

function optionfilter(f, _param) {
  while(true) {
    var param = _param;
    if (param) {
      var xs = param[1];
      var match = Curry._1(f, param[0]);
      if (match !== undefined) {
        return /* :: */[
                Caml_option.valFromOption(match),
                optionfilter(f, xs)
              ];
      } else {
        _param = xs;
        continue ;
      }
    } else {
      return /* [] */0;
    }
  };
}

function option_foldl(f, z, param) {
  if (param) {
    var xs = param[1];
    return $amp$tilde$tilde(Curry._2(f, z, param[0]), (function (z$prime) {
                  return option_foldl(f, z$prime, xs);
                }));
  } else {
    return Caml_option.some(z);
  }
}

function option_foldr(f, z, param) {
  if (param) {
    return $amp$tilde$tilde(option_foldr(f, z, param[1]), Curry._1(f, param[0]));
  } else {
    return Caml_option.some(z);
  }
}

function option_njfold(f, param) {
  if (param) {
    var x = param[0];
    var partial_arg = option_njfold(f, param[1]);
    return (function (param) {
        return $amp$tilde$tilde(Curry._1(partial_arg, param), (function (param) {
                      return Miscellaneous$ReasonReactExamples.curry2(f, x, param);
                    }));
      });
  } else {
    return _Some;
  }
}

function findfirst(f, _param) {
  while(true) {
    var param = _param;
    if (param) {
      var t = Curry._1(f, param[0]);
      if (t !== undefined) {
        return t;
      } else {
        _param = param[1];
        continue ;
      }
    } else {
      return ;
    }
  };
}

function findbest(f, best, _param) {
  while(true) {
    var param = _param;
    if (param) {
      var xs = param[1];
      var match = Curry._1(f, param[0]);
      if (match !== undefined) {
        var y1 = Caml_option.valFromOption(match);
        var match$1 = findbest(f, best, xs);
        if (match$1 !== undefined) {
          return Caml_option.some(Curry._2(best, y1, Caml_option.valFromOption(match$1)));
        } else {
          return Caml_option.some(y1);
        }
      } else {
        _param = xs;
        continue ;
      }
    } else {
      return ;
    }
  };
}

function stripoption(param) {
  if (param !== undefined) {
    return Caml_option.valFromOption(param);
  }
  
}

function optordefault(param) {
  var match = param[0];
  if (match !== undefined) {
    return Caml_option.valFromOption(match);
  } else {
    return param[1];
  }
}

function catelim_string_of_option(catelim_astring, aopt, ss) {
  if (aopt !== undefined) {
    return /* :: */[
            "Some (",
            Curry._2(catelim_astring, Caml_option.valFromOption(aopt), /* :: */[
                  ")",
                  ss
                ])
          ];
  } else {
    return /* :: */[
            "None",
            ss
          ];
  }
}

function string_of_option(astring, aopt) {
  return Sml$ReasonReactExamples.implode(catelim_string_of_option((function (a, ss) {
                    return /* :: */[
                            Curry._1(astring, a),
                            ss
                          ];
                  }), aopt, /* [] */0));
}

function option_rewrite2(fa, fb, param) {
  var b = param[1];
  var a = param[0];
  var match = Curry._1(fa, a);
  var match$1 = Curry._1(fb, b);
  if (match !== undefined) {
    var a$1 = Caml_option.valFromOption(match);
    if (match$1 !== undefined) {
      return /* tuple */[
              a$1,
              Caml_option.valFromOption(match$1)
            ];
    } else {
      return /* tuple */[
              a$1,
              b
            ];
    }
  } else if (match$1 !== undefined) {
    return /* tuple */[
            a,
            Caml_option.valFromOption(match$1)
          ];
  } else {
    return ;
  }
}

function option_rewrite3(fa, fb, fc, param) {
  var c = param[2];
  var b = param[1];
  var a = param[0];
  var match = Curry._1(fa, a);
  var match$1 = Curry._1(fb, b);
  var match$2 = Curry._1(fc, c);
  if (match !== undefined) {
    var a$1 = Caml_option.valFromOption(match);
    if (match$1 !== undefined) {
      var b$1 = Caml_option.valFromOption(match$1);
      if (match$2 !== undefined) {
        return /* tuple */[
                a$1,
                b$1,
                Caml_option.valFromOption(match$2)
              ];
      } else {
        return /* tuple */[
                a$1,
                b$1,
                c
              ];
      }
    } else if (match$2 !== undefined) {
      return /* tuple */[
              a$1,
              b,
              Caml_option.valFromOption(match$2)
            ];
    } else {
      return /* tuple */[
              a$1,
              b,
              c
            ];
    }
  } else if (match$1 !== undefined) {
    var b$2 = Caml_option.valFromOption(match$1);
    if (match$2 !== undefined) {
      return /* tuple */[
              a,
              b$2,
              Caml_option.valFromOption(match$2)
            ];
    } else {
      return /* tuple */[
              a,
              b$2,
              c
            ];
    }
  } else if (match$2 !== undefined) {
    return /* tuple */[
            a,
            b,
            Caml_option.valFromOption(match$2)
          ];
  } else {
    return ;
  }
}

function option_rewritelist(f, xs) {
  if (xs) {
    var xs$1 = xs[1];
    var x = xs[0];
    var match = Curry._1(f, x);
    var match$1 = option_rewritelist(f, xs$1);
    if (match !== undefined) {
      var x$1 = Caml_option.valFromOption(match);
      if (match$1 !== undefined) {
        return /* :: */[
                x$1,
                match$1
              ];
      } else {
        return /* :: */[
                x$1,
                xs$1
              ];
      }
    } else if (match$1 !== undefined) {
      return /* :: */[
              x,
              match$1
            ];
    } else {
      return ;
    }
  }
  
}

exports.None_ = None_;
exports._The = _The;
exports._Some = _Some;
exports.bool_of_opt = bool_of_opt;
exports.optf = optf;
exports.somef = somef;
exports.anyway = anyway;
exports.failpt = failpt;
exports.$amp$tilde$tilde = $amp$tilde$tilde;
exports.$pipe$tilde$tilde = $pipe$tilde$tilde;
exports.$amp$tilde = $amp$tilde;
exports.$pipe$tilde = $pipe$tilde;
exports.optioncompose = optioncompose;
exports.optionmap = optionmap;
exports.optionfilter = optionfilter;
exports.option_foldl = option_foldl;
exports.option_foldr = option_foldr;
exports.option_njfold = option_njfold;
exports.findfirst = findfirst;
exports.findbest = findbest;
exports.stripoption = stripoption;
exports.optordefault = optordefault;
exports.catelim_string_of_option = catelim_string_of_option;
exports.string_of_option = string_of_option;
exports.option_rewrite2 = option_rewrite2;
exports.option_rewrite3 = option_rewrite3;
exports.option_rewritelist = option_rewritelist;
/* No side effect */
