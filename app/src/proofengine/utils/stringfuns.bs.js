'use strict';

var Char = require("bs-platform/lib/js/char.js");
var List = require("bs-platform/lib/js/list.js");
var Curry = require("bs-platform/lib/js/curry.js");
var $$String = require("bs-platform/lib/js/string.js");
var Caml_array = require("bs-platform/lib/js/caml_array.js");
var Caml_string = require("bs-platform/lib/js/caml_string.js");
var Sml$ReasonReactExamples = require("./sml.bs.js");
var Listfuns$ReasonReactExamples = require("./listfuns.bs.js");

function isQuoted(s) {
  if ($$String.sub(s, 0, 1) === "\"") {
    return $$String.sub(s, s.length - 1 | 0, 1) === "\"";
  } else {
    return false;
  }
}

function disQuote(s) {
  try {
    var size = s.length;
    var match = $$String.sub(s, 0, 1);
    if (match === "\"") {
      var match$1 = $$String.sub(s, size - 1 | 0, 1);
      if (match$1 === "\"") {
        return $$String.sub(s, 1, size - 2 | 0);
      } else {
        return s;
      }
    } else {
      return s;
    }
  }
  catch (exn){
    return s;
  }
}

function enQuote(s) {
  return "\"" + (Sml$ReasonReactExamples.implode(List.map((function (s) {
                      if (s === "\"") {
                        return "\\\"";
                      } else {
                        return s;
                      }
                    }), Sml$ReasonReactExamples.explode(s))) + "\"");
}

function enCharQuote(s) {
  return "'" + (Sml$ReasonReactExamples.implode(List.map((function (s) {
                      if (s === "'") {
                        return "\\'";
                      } else {
                        return s;
                      }
                    }), Sml$ReasonReactExamples.explode(s))) + "'");
}

function catelim_string_of_pair(fa, fb, sep, param, tail) {
  return /* :: */[
          "(",
          Curry._2(fa, param[0], /* :: */[
                sep,
                Curry._2(fb, param[1], /* :: */[
                      ")",
                      tail
                    ])
              ])
        ];
}

function catelim_string_of_triple(fa, fb, fc, sep, param, tail) {
  return /* :: */[
          "(",
          Curry._2(fa, param[0], /* :: */[
                sep,
                Curry._2(fb, param[1], /* :: */[
                      sep,
                      Curry._2(fc, param[2], /* :: */[
                            ")",
                            tail
                          ])
                    ])
              ])
        ];
}

function catelim_string_of_quadruple(fa, fb, fc, fd, sep, param, tail) {
  return /* :: */[
          "(",
          Curry._2(fa, param[0], /* :: */[
                sep,
                Curry._2(fb, param[1], /* :: */[
                      sep,
                      Curry._2(fc, param[2], /* :: */[
                            sep,
                            Curry._2(fd, param[3], /* :: */[
                                  ")",
                                  tail
                                ])
                          ])
                    ])
              ])
        ];
}

function catelim_string_of_quintuple(fa, fb, fc, fd, fe, sep, param, tail) {
  return /* :: */[
          "(",
          Curry._2(fa, param[0], /* :: */[
                sep,
                Curry._2(fb, param[1], /* :: */[
                      sep,
                      Curry._2(fc, param[2], /* :: */[
                            sep,
                            Curry._2(fd, param[3], /* :: */[
                                  sep,
                                  Curry._2(fe, param[4], /* :: */[
                                        ")",
                                        tail
                                      ])
                                ])
                          ])
                    ])
              ])
        ];
}

function catelim_string_of_sextuple(fa, fb, fc, fd, fe, ff, sep, param, tail) {
  return /* :: */[
          "(",
          Curry._2(fa, param[0], /* :: */[
                sep,
                Curry._2(fb, param[1], /* :: */[
                      sep,
                      Curry._2(fc, param[2], /* :: */[
                            sep,
                            Curry._2(fd, param[3], /* :: */[
                                  sep,
                                  Curry._2(fe, param[4], /* :: */[
                                        sep,
                                        Curry._2(ff, param[5], /* :: */[
                                              ")",
                                              tail
                                            ])
                                      ])
                                ])
                          ])
                    ])
              ])
        ];
}

function catelim_string_of_septuple(fa, fb, fc, fd, fe, ff, fg, sep, param, tail) {
  return /* :: */[
          "(",
          Curry._2(fa, param[0], /* :: */[
                sep,
                Curry._2(fb, param[1], /* :: */[
                      sep,
                      Curry._2(fc, param[2], /* :: */[
                            sep,
                            Curry._2(fd, param[3], /* :: */[
                                  sep,
                                  Curry._2(fe, param[4], /* :: */[
                                        sep,
                                        Curry._2(ff, param[5], /* :: */[
                                              sep,
                                              Curry._2(fg, param[6], /* :: */[
                                                    ")",
                                                    tail
                                                  ])
                                            ])
                                      ])
                                ])
                          ])
                    ])
              ])
        ];
}

function catelim_string_of_octuple(fa, fb, fc, fd, fe, ff, fg, fh, sep, param, tail) {
  return /* :: */[
          "(",
          Curry._2(fa, param[0], /* :: */[
                sep,
                Curry._2(fb, param[1], /* :: */[
                      sep,
                      Curry._2(fc, param[2], /* :: */[
                            sep,
                            Curry._2(fd, param[3], /* :: */[
                                  sep,
                                  Curry._2(fe, param[4], /* :: */[
                                        sep,
                                        Curry._2(ff, param[5], /* :: */[
                                              sep,
                                              Curry._2(fg, param[6], /* :: */[
                                                    sep,
                                                    Curry._2(fh, param[7], /* :: */[
                                                          ")",
                                                          tail
                                                        ])
                                                  ])
                                            ])
                                      ])
                                ])
                          ])
                    ])
              ])
        ];
}

function string_of_pair(fa, fb, sep) {
  return (function (param) {
      return Listfuns$ReasonReactExamples.stringfn_of_catelim((function (param, param$1) {
                    return catelim_string_of_pair((function (param, param$1) {
                                  return Listfuns$ReasonReactExamples.catelim_of_stringfn(fa, param, param$1);
                                }), (function (param, param$1) {
                                  return Listfuns$ReasonReactExamples.catelim_of_stringfn(fb, param, param$1);
                                }), sep, param, param$1);
                  }), param);
    });
}

function string_of_triple(fa, fb, fc, sep) {
  return (function (param) {
      return Listfuns$ReasonReactExamples.stringfn_of_catelim((function (param, param$1) {
                    return catelim_string_of_triple((function (param, param$1) {
                                  return Listfuns$ReasonReactExamples.catelim_of_stringfn(fa, param, param$1);
                                }), (function (param, param$1) {
                                  return Listfuns$ReasonReactExamples.catelim_of_stringfn(fb, param, param$1);
                                }), (function (param, param$1) {
                                  return Listfuns$ReasonReactExamples.catelim_of_stringfn(fc, param, param$1);
                                }), sep, param, param$1);
                  }), param);
    });
}

function string_of_quadruple(fa, fb, fc, fd, sep) {
  return (function (param) {
      return Listfuns$ReasonReactExamples.stringfn_of_catelim((function (param, param$1) {
                    return catelim_string_of_quadruple((function (param, param$1) {
                                  return Listfuns$ReasonReactExamples.catelim_of_stringfn(fa, param, param$1);
                                }), (function (param, param$1) {
                                  return Listfuns$ReasonReactExamples.catelim_of_stringfn(fb, param, param$1);
                                }), (function (param, param$1) {
                                  return Listfuns$ReasonReactExamples.catelim_of_stringfn(fc, param, param$1);
                                }), (function (param, param$1) {
                                  return Listfuns$ReasonReactExamples.catelim_of_stringfn(fd, param, param$1);
                                }), sep, param, param$1);
                  }), param);
    });
}

function string_of_quintuple(fa, fb, fc, fd, fe, sep) {
  return (function (param) {
      return Listfuns$ReasonReactExamples.stringfn_of_catelim((function (param, param$1) {
                    return catelim_string_of_quintuple((function (param, param$1) {
                                  return Listfuns$ReasonReactExamples.catelim_of_stringfn(fa, param, param$1);
                                }), (function (param, param$1) {
                                  return Listfuns$ReasonReactExamples.catelim_of_stringfn(fb, param, param$1);
                                }), (function (param, param$1) {
                                  return Listfuns$ReasonReactExamples.catelim_of_stringfn(fc, param, param$1);
                                }), (function (param, param$1) {
                                  return Listfuns$ReasonReactExamples.catelim_of_stringfn(fd, param, param$1);
                                }), (function (param, param$1) {
                                  return Listfuns$ReasonReactExamples.catelim_of_stringfn(fe, param, param$1);
                                }), sep, param, param$1);
                  }), param);
    });
}

function string_of_sextuple(fa, fb, fc, fd, fe, ff, sep) {
  return (function (param) {
      return Listfuns$ReasonReactExamples.stringfn_of_catelim((function (param, param$1) {
                    return catelim_string_of_sextuple((function (param, param$1) {
                                  return Listfuns$ReasonReactExamples.catelim_of_stringfn(fa, param, param$1);
                                }), (function (param, param$1) {
                                  return Listfuns$ReasonReactExamples.catelim_of_stringfn(fb, param, param$1);
                                }), (function (param, param$1) {
                                  return Listfuns$ReasonReactExamples.catelim_of_stringfn(fc, param, param$1);
                                }), (function (param, param$1) {
                                  return Listfuns$ReasonReactExamples.catelim_of_stringfn(fd, param, param$1);
                                }), (function (param, param$1) {
                                  return Listfuns$ReasonReactExamples.catelim_of_stringfn(fe, param, param$1);
                                }), (function (param, param$1) {
                                  return Listfuns$ReasonReactExamples.catelim_of_stringfn(ff, param, param$1);
                                }), sep, param, param$1);
                  }), param);
    });
}

function string_of_septuple(fa, fb, fc, fd, fe, ff, fg, sep) {
  return (function (param) {
      return Listfuns$ReasonReactExamples.stringfn_of_catelim((function (param, param$1) {
                    return catelim_string_of_septuple((function (param, param$1) {
                                  return Listfuns$ReasonReactExamples.catelim_of_stringfn(fa, param, param$1);
                                }), (function (param, param$1) {
                                  return Listfuns$ReasonReactExamples.catelim_of_stringfn(fb, param, param$1);
                                }), (function (param, param$1) {
                                  return Listfuns$ReasonReactExamples.catelim_of_stringfn(fc, param, param$1);
                                }), (function (param, param$1) {
                                  return Listfuns$ReasonReactExamples.catelim_of_stringfn(fd, param, param$1);
                                }), (function (param, param$1) {
                                  return Listfuns$ReasonReactExamples.catelim_of_stringfn(fe, param, param$1);
                                }), (function (param, param$1) {
                                  return Listfuns$ReasonReactExamples.catelim_of_stringfn(ff, param, param$1);
                                }), (function (param, param$1) {
                                  return Listfuns$ReasonReactExamples.catelim_of_stringfn(fg, param, param$1);
                                }), sep, param, param$1);
                  }), param);
    });
}

function string_of_octuple(fa, fb, fc, fd, fe, ff, fg, fh, sep) {
  return (function (param) {
      return Listfuns$ReasonReactExamples.stringfn_of_catelim((function (param, param$1) {
                    return catelim_string_of_octuple((function (param, param$1) {
                                  return Listfuns$ReasonReactExamples.catelim_of_stringfn(fa, param, param$1);
                                }), (function (param, param$1) {
                                  return Listfuns$ReasonReactExamples.catelim_of_stringfn(fb, param, param$1);
                                }), (function (param, param$1) {
                                  return Listfuns$ReasonReactExamples.catelim_of_stringfn(fc, param, param$1);
                                }), (function (param, param$1) {
                                  return Listfuns$ReasonReactExamples.catelim_of_stringfn(fd, param, param$1);
                                }), (function (param, param$1) {
                                  return Listfuns$ReasonReactExamples.catelim_of_stringfn(fe, param, param$1);
                                }), (function (param, param$1) {
                                  return Listfuns$ReasonReactExamples.catelim_of_stringfn(ff, param, param$1);
                                }), (function (param, param$1) {
                                  return Listfuns$ReasonReactExamples.catelim_of_stringfn(fg, param, param$1);
                                }), (function (param, param$1) {
                                  return Listfuns$ReasonReactExamples.catelim_of_stringfn(fh, param, param$1);
                                }), sep, param, param$1);
                  }), param);
    });
}

function catelim_string_of_array(f, sep, a, ss) {
  var el = function (i, ss) {
    if (i === a.length) {
      return ss;
    } else {
      var ss$1 = i === (a.length - 1 | 0) ? ss : /* :: */[
          sep,
          el(i + 1 | 0, ss)
        ];
      return /* :: */[
              String(i),
              /* :: */[
                ": ",
                Curry._2(f, Caml_array.caml_array_get(a, i), ss$1)
              ]
            ];
    }
  };
  return /* :: */[
          "\xef\xbf\xbd",
          el(0, /* :: */[
                "\xef\xbf\xbd",
                ss
              ])
        ];
}

function string_of_array(f, sep) {
  return (function (param) {
      return Listfuns$ReasonReactExamples.stringfn_of_catelim((function (param, param$1) {
                    return catelim_string_of_array((function (param, param$1) {
                                  return Listfuns$ReasonReactExamples.catelim_of_stringfn(f, param, param$1);
                                }), sep, param, param$1);
                  }), param);
    });
}

function quotedstring_of_char(c) {
  return "'" + (Char.escaped(c) + "'");
}

var hexdigs = "0123456789abcdef";

function fixedwidth_hexstring_of_int(w, i) {
  var h = function (_w, _i, _cs) {
    while(true) {
      var cs = _cs;
      var i = _i;
      var w = _w;
      if (i === 0 && w <= 0) {
        return cs;
      } else {
        _cs = /* :: */[
          Caml_string.get(hexdigs, i & 15),
          cs
        ];
        _i = (i >>> 4);
        _w = w - 1 | 0;
        continue ;
      }
    };
  };
  return Sml$ReasonReactExamples.string_of_chars(h(w, i, /* [] */0));
}

function hexstring_of_int(param) {
  return fixedwidth_hexstring_of_int(1, param);
}

var lowercase = $$String.lowercase_ascii;

var uppercase = $$String.uppercase_ascii;

var s = Listfuns$ReasonReactExamples.catelim_of_stringfn;

exports.isQuoted = isQuoted;
exports.disQuote = disQuote;
exports.enQuote = enQuote;
exports.enCharQuote = enCharQuote;
exports.lowercase = lowercase;
exports.uppercase = uppercase;
exports.catelim_string_of_pair = catelim_string_of_pair;
exports.catelim_string_of_triple = catelim_string_of_triple;
exports.catelim_string_of_quadruple = catelim_string_of_quadruple;
exports.catelim_string_of_quintuple = catelim_string_of_quintuple;
exports.catelim_string_of_sextuple = catelim_string_of_sextuple;
exports.catelim_string_of_septuple = catelim_string_of_septuple;
exports.catelim_string_of_octuple = catelim_string_of_octuple;
exports.s = s;
exports.string_of_pair = string_of_pair;
exports.string_of_triple = string_of_triple;
exports.string_of_quadruple = string_of_quadruple;
exports.string_of_quintuple = string_of_quintuple;
exports.string_of_sextuple = string_of_sextuple;
exports.string_of_septuple = string_of_septuple;
exports.string_of_octuple = string_of_octuple;
exports.catelim_string_of_array = catelim_string_of_array;
exports.string_of_array = string_of_array;
exports.quotedstring_of_char = quotedstring_of_char;
exports.hexdigs = hexdigs;
exports.fixedwidth_hexstring_of_int = fixedwidth_hexstring_of_int;
exports.hexstring_of_int = hexstring_of_int;
/* No side effect */
