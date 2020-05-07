'use strict';

var List = require("bs-platform/lib/js/list.js");
var Curry = require("bs-platform/lib/js/curry.js");
var Caml_obj = require("bs-platform/lib/js/caml_obj.js");
var Caml_option = require("bs-platform/lib/js/caml_option.js");
var Caml_exceptions = require("bs-platform/lib/js/caml_exceptions.js");
var Caml_js_exceptions = require("bs-platform/lib/js/caml_js_exceptions.js");
var Caml_builtin_exceptions = require("bs-platform/lib/js/caml_builtin_exceptions.js");
var Sml$ReasonReactExamples = require("./sml.bs.js");

var First = Caml_exceptions.create("Listfuns-ReasonReactExamples.First");

var Reduce = Caml_exceptions.create("Listfuns-ReasonReactExamples.Reduce");

function foldr(f, z, param) {
  if (param) {
    return Curry._2(f, param[0], foldr(f, z, param[1]));
  } else {
    return z;
  }
}

function foldl(f, _z, _param) {
  while(true) {
    var param = _param;
    var z = _z;
    if (param) {
      _param = param[1];
      _z = Curry._2(f, z, param[0]);
      continue ;
    } else {
      return z;
    }
  };
}

var Zip_ = Caml_exceptions.create("Listfuns-ReasonReactExamples.Zip_");

function $pipe$pipe$pipe(xs, ys) {
  try {
    return List.combine(xs, ys);
  }
  catch (raw_exn){
    var exn = Caml_js_exceptions.internalToOCamlException(raw_exn);
    if (exn[0] === Caml_builtin_exceptions.invalid_argument) {
      throw Zip_;
    }
    throw exn;
  }
}

function doubleslosh(param) {
  var pp = param[1];
  var f = param[0];
  var ff = function (_param) {
    while(true) {
      var param = _param;
      if (param) {
        var xs = param[1];
        var x = param[0];
        if (Curry._1(pp, x)) {
          return /* :: */[
                  Curry._1(f, x),
                  ff(xs)
                ];
        } else {
          _param = xs;
          continue ;
        }
      } else {
        return /* [] */0;
      }
    };
  };
  return ff;
}

function $slash$great(param) {
  var $plus$plus = param[1];
  var e = param[0];
  return (function (param) {
      var _a1 = e;
      var _a2 = param;
      while(true) {
        var a2 = _a2;
        var a1 = _a1;
        if (a2) {
          _a2 = a2[1];
          _a1 = Curry._1($plus$plus, /* tuple */[
                a1,
                a2[0]
              ]);
          continue ;
        } else {
          return a1;
        }
      };
    });
}

function $less$slash(param) {
  var e = param[1];
  var $plus$plus = param[0];
  var ff = function (param) {
    if (param) {
      return Curry._1($plus$plus, /* tuple */[
                  param[0],
                  ff(param[1])
                ]);
    } else {
      return e;
    }
  };
  return ff;
}

function $slash$slash(param) {
  var match = param[1];
  if (match) {
    return $slash$great(/* tuple */[
                  match[0],
                  param[0]
                ])(match[1]);
  } else {
    throw Reduce;
  }
}

function all(f) {
  return (function (param) {
      return Sml$ReasonReactExamples.$less$dot$great((function (prim) {
                    return !prim;
                  }), (function (param) {
                    return List.exists((function (param) {
                                  return Sml$ReasonReactExamples.$less$dot$great((function (prim) {
                                                return !prim;
                                              }), f, param);
                                }), param);
                  }), param);
    });
}

function all1(f, xs) {
  if (all(f)(xs)) {
    return !Sml$ReasonReactExamples.$$null(xs);
  } else {
    return false;
  }
}

function member(param) {
  return List.mem(param[0], param[1]);
}

function nonmember(param) {
  return !member(/* tuple */[
              param[0],
              param[1]
            ]);
}

function slosh(param) {
  var tt = param[1];
  return List.filter((function (x) {
                  return nonmember(/* tuple */[
                              x,
                              tt
                            ]);
                }))(param[0]);
}

function seteq(eq, xs) {
  return Sml$ReasonReactExamples.nj_revfold((function (param) {
                var ys = param[1];
                var x = param[0];
                if (List.exists((function (x$prime) {
                          return Curry._2(eq, x, x$prime);
                        }), ys)) {
                  return ys;
                } else {
                  return /* :: */[
                          x,
                          ys
                        ];
                }
              }), xs, /* [] */0);
}

function set(xs) {
  return seteq(Caml_obj.caml_equal, xs);
}

function subset(param) {
  var ys = param[1];
  return all((function (x) {
                  return member(/* tuple */[
                              x,
                              ys
                            ]);
                }))(param[0]);
}

function _INTER(xs, ys) {
  return List.filter((function (y) {
                  return member(/* tuple */[
                              y,
                              xs
                            ]);
                }))(ys);
}

function interpolate(sep, ss) {
  if (ss) {
    var ss$1 = ss[1];
    var s = ss[0];
    if (ss$1) {
      return /* :: */[
              s,
              /* :: */[
                sep,
                interpolate(sep, ss$1)
              ]
            ];
    } else {
      return /* :: */[
              s,
              /* [] */0
            ];
    }
  } else {
    return /* [] */0;
  }
}

function catelim_interpolate(f, sep, xs, ys) {
  if (xs) {
    var xs$1 = xs[1];
    var x = xs[0];
    if (xs$1) {
      return Curry._2(f, x, /* :: */[
                  sep,
                  catelim_interpolate(f, sep, xs$1, ys)
                ]);
    } else {
      return Curry._2(f, x, ys);
    }
  } else {
    return ys;
  }
}

function stringfn_of_catelim(f, x) {
  return Sml$ReasonReactExamples.implode(Curry._2(f, x, /* [] */0));
}

function catelim_of_stringfn(f, x, ss) {
  return /* :: */[
          Curry._1(f, x),
          ss
        ];
}

function catelim_string_of_list(obstring, punct) {
  return (function (param, param$1) {
      return catelim_interpolate(obstring, punct, param, param$1);
    });
}

function catelim_sentencestring_of_list(obstring, sepn, sep2, xs, tail) {
  if (xs) {
    var xs$1 = xs[1];
    var x = xs[0];
    if (xs$1) {
      if (xs$1[1]) {
        return Curry._2(obstring, x, /* :: */[
                    sepn,
                    catelim_sentencestring_of_list(obstring, sepn, sep2, xs$1, tail)
                  ]);
      } else {
        return Curry._2(obstring, x, /* :: */[
                    sep2,
                    Curry._2(obstring, xs$1[0], tail)
                  ]);
      }
    } else {
      return Curry._2(obstring, x, tail);
    }
  } else {
    return tail;
  }
}

function catelim_bracketedstring_of_list(obstring, punct, xs, tail) {
  return /* :: */[
          "[",
          catelim_string_of_list(obstring, punct)(xs, /* :: */[
                "]",
                tail
              ])
        ];
}

function string_of_list(obstring, punct) {
  return (function (param) {
      return Sml$ReasonReactExamples.implode(catelim_interpolate((function (param, param$1) {
                        return /* :: */[
                                Curry._1(obstring, param),
                                param$1
                              ];
                      }), punct, param, /* [] */0));
    });
}

function sentencestring_of_list(obstring, sepn, sep2) {
  return (function (param) {
      return Sml$ReasonReactExamples.implode(catelim_sentencestring_of_list((function (param, param$1) {
                        return /* :: */[
                                Curry._1(obstring, param),
                                param$1
                              ];
                      }), sepn, sep2, param, /* [] */0));
    });
}

function bracketed_string_of_list(obstring, punct) {
  return (function (param) {
      return Sml$ReasonReactExamples.implode(catelim_bracketedstring_of_list((function (param, param$1) {
                        return /* :: */[
                                Curry._1(obstring, param),
                                param$1
                              ];
                      }), punct, param, /* [] */0));
    });
}

function replacenth(xs, n, y) {
  if (xs) {
    var xs$1 = xs[1];
    if (n !== 0) {
      return /* :: */[
              xs[0],
              replacenth(xs$1, n - 1 | 0, y)
            ];
    } else {
      return /* :: */[
              y,
              xs$1
            ];
    }
  } else {
    return /* [] */0;
  }
}

var Bad_nth = Caml_exceptions.create("Listfuns-ReasonReactExamples.Bad_nth");

function guardednth(xs, n) {
  try {
    return List.nth(xs, n);
  }
  catch (raw_exn){
    var exn = Caml_js_exceptions.internalToOCamlException(raw_exn);
    if (exn[0] === Caml_builtin_exceptions.invalid_argument) {
      throw Bad_nth;
    }
    if (exn[0] === Caml_builtin_exceptions.failure) {
      throw Bad_nth;
    }
    throw exn;
  }
}

var Last_ = Caml_exceptions.create("Listfuns-ReasonReactExamples.Last_");

function last(_param) {
  while(true) {
    var param = _param;
    if (param) {
      var xs = param[1];
      if (xs) {
        _param = xs;
        continue ;
      } else {
        return param[0];
      }
    } else {
      throw Last_;
    }
  };
}

function take(a1, a2) {
  if (a1 !== 0 && a2) {
    return /* :: */[
            a2[0],
            take(a1 - 1 | 0, a2[1])
          ];
  } else {
    return /* [] */0;
  }
}

function drop(_a1, _a2) {
  while(true) {
    var a2 = _a2;
    var a1 = _a1;
    if (a1 !== 0) {
      if (a2) {
        _a2 = a2[1];
        _a1 = a1 - 1 | 0;
        continue ;
      } else {
        return /* [] */0;
      }
    } else {
      return a2;
    }
  };
}

function takewhile(a1, a2) {
  if (a2) {
    var x = a2[0];
    if (Curry._1(a1, x)) {
      return /* :: */[
              x,
              takewhile(a1, a2[1])
            ];
    } else {
      return /* [] */0;
    }
  } else {
    return /* [] */0;
  }
}

function dropwhile(a1, _a2) {
  while(true) {
    var a2 = _a2;
    if (a2) {
      var xs = a2[1];
      var x = a2[0];
      if (Curry._1(a1, x)) {
        _a2 = xs;
        continue ;
      } else {
        return /* :: */[
                x,
                xs
              ];
      }
    } else {
      return /* [] */0;
    }
  };
}

function _BMzip(xs, ys) {
  if (xs && ys) {
    return /* :: */[
            /* tuple */[
              xs[0],
              ys[0]
            ],
            _BMzip(xs[1], ys[1])
          ];
  } else {
    return /* [] */0;
  }
}

function isprefix(eq, _xs, _ys) {
  while(true) {
    var ys = _ys;
    var xs = _xs;
    if (xs) {
      if (ys && Curry._1(eq, /* tuple */[
              xs[0],
              ys[0]
            ])) {
        _ys = ys[1];
        _xs = xs[1];
        continue ;
      } else {
        return false;
      }
    } else {
      return true;
    }
  };
}

var Extract_ = Caml_exceptions.create("Listfuns-ReasonReactExamples.Extract_");

function extract(f, param) {
  if (param) {
    var xs = param[1];
    var x = param[0];
    if (Curry._1(f, x)) {
      return /* tuple */[
              x,
              xs
            ];
    } else {
      var match = extract(f, xs);
      return /* tuple */[
              match[0],
              /* :: */[
                x,
                match[1]
              ]
            ];
    }
  } else {
    throw Extract_;
  }
}

function split(f, param) {
  if (param) {
    var x = param[0];
    var match = split(f, param[1]);
    var nos = match[1];
    var yess = match[0];
    if (Curry._1(f, x)) {
      return /* tuple */[
              /* :: */[
                x,
                yess
              ],
              nos
            ];
    } else {
      return /* tuple */[
              yess,
              /* :: */[
                x,
                nos
              ]
            ];
    }
  } else {
    return /* tuple */[
            /* [] */0,
            /* [] */0
          ];
  }
}

var Matchinmergepairs = Caml_exceptions.create("Listfuns-ReasonReactExamples.Matchinmergepairs");

function sort($less, ls) {
  var merge = function (param) {
    var xs = param[0];
    if (xs) {
      var match = param[1];
      if (match) {
        var ys = match[1];
        var y = match[0];
        var xs$1 = xs[1];
        var x = xs[0];
        if (Curry._2($less, x, y)) {
          return /* :: */[
                  x,
                  merge(/* tuple */[
                        xs$1,
                        /* :: */[
                          y,
                          ys
                        ]
                      ])
                ];
        } else {
          return /* :: */[
                  y,
                  merge(/* tuple */[
                        /* :: */[
                          x,
                          xs$1
                        ],
                        ys
                      ])
                ];
        }
      } else {
        return xs;
      }
    } else {
      return param[1];
    }
  };
  var mergepairs = function (_param) {
    while(true) {
      var param = _param;
      var ls = param[0];
      if (ls) {
        var match = ls[1];
        if (match) {
          var l = ls[0];
          var k = param[1];
          var ls$1 = match[1];
          var l2 = match[0];
          if (k % 2 === 1) {
            return /* :: */[
                    l,
                    /* :: */[
                      l2,
                      ls$1
                    ]
                  ];
          } else {
            _param = /* tuple */[
              /* :: */[
                merge(/* tuple */[
                      l,
                      l2
                    ]),
                ls$1
              ],
              k / 2 | 0
            ];
            continue ;
          }
        } else {
          return ls;
        }
      } else {
        throw Matchinmergepairs;
      }
    };
  };
  var nextrun = function (_param) {
    while(true) {
      var param = _param;
      var match = param[1];
      var run = param[0];
      if (match) {
        var xs = match[1];
        var x = match[0];
        if (Curry._2($less, x, List.hd(run))) {
          _param = /* tuple */[
            /* :: */[
              x,
              run
            ],
            xs
          ];
          continue ;
        } else {
          return /* tuple */[
                  run,
                  /* :: */[
                    x,
                    xs
                  ]
                ];
        }
      } else {
        return /* tuple */[
                run,
                /* [] */0
              ];
      }
    };
  };
  if (ls) {
    var _param = /* tuple */[
      ls,
      /* [] */0,
      0
    ];
    while(true) {
      var param = _param;
      var match = param[0];
      if (match) {
        var k = param[2];
        var match$1 = nextrun(/* tuple */[
              /* :: */[
                match[0],
                /* [] */0
              ],
              match[1]
            ]);
        _param = /* tuple */[
          match$1[1],
          mergepairs(/* tuple */[
                /* :: */[
                  match$1[0],
                  param[1]
                ],
                k + 1 | 0
              ]),
          k + 1 | 0
        ];
        continue ;
      } else {
        return List.hd(mergepairs(/* tuple */[
                        param[1],
                        0
                      ]));
      }
    };
  } else {
    return /* [] */0;
  }
}

function sortandcombine($less, $plus$plus, ls) {
  var merge = function (param) {
    var xs = param[0];
    if (xs) {
      var match = param[1];
      if (match) {
        var ys = match[1];
        var y = match[0];
        var xs$1 = xs[1];
        var x = xs[0];
        if (Curry._2($less, x, y)) {
          return /* :: */[
                  x,
                  merge(/* tuple */[
                        xs$1,
                        /* :: */[
                          y,
                          ys
                        ]
                      ])
                ];
        } else if (Curry._2($less, y, x)) {
          return /* :: */[
                  y,
                  merge(/* tuple */[
                        /* :: */[
                          x,
                          xs$1
                        ],
                        ys
                      ])
                ];
        } else {
          return /* :: */[
                  Curry._1($plus$plus, /* tuple */[
                        x,
                        y
                      ]),
                  merge(/* tuple */[
                        xs$1,
                        ys
                      ])
                ];
        }
      } else {
        return xs;
      }
    } else {
      return param[1];
    }
  };
  var mergepairs = function (_param) {
    while(true) {
      var param = _param;
      var ls = param[0];
      if (ls) {
        var match = ls[1];
        if (match) {
          var l = ls[0];
          var k = param[1];
          var ls$1 = match[1];
          var l2 = match[0];
          if (k % 2 === 1) {
            return /* :: */[
                    l,
                    /* :: */[
                      l2,
                      ls$1
                    ]
                  ];
          } else {
            _param = /* tuple */[
              /* :: */[
                merge(/* tuple */[
                      l,
                      l2
                    ]),
                ls$1
              ],
              k / 2 | 0
            ];
            continue ;
          }
        } else {
          return ls;
        }
      } else {
        throw Matchinmergepairs;
      }
    };
  };
  var nextrun = function (_param) {
    while(true) {
      var param = _param;
      var match = param[1];
      var run = param[0];
      if (match) {
        var xs = match[1];
        var x = match[0];
        if (Curry._2($less, x, List.hd(run))) {
          _param = /* tuple */[
            /* :: */[
              x,
              run
            ],
            xs
          ];
          continue ;
        } else {
          return /* tuple */[
                  run,
                  /* :: */[
                    x,
                    xs
                  ]
                ];
        }
      } else {
        return /* tuple */[
                run,
                /* [] */0
              ];
      }
    };
  };
  if (ls) {
    var _param = /* tuple */[
      ls,
      /* [] */0,
      0
    ];
    while(true) {
      var param = _param;
      var match = param[0];
      if (match) {
        var k = param[2];
        var match$1 = nextrun(/* tuple */[
              /* :: */[
                match[0],
                /* [] */0
              ],
              match[1]
            ]);
        _param = /* tuple */[
          match$1[1],
          mergepairs(/* tuple */[
                /* :: */[
                  match$1[0],
                  param[1]
                ],
                k + 1 | 0
              ]),
          k + 1 | 0
        ];
        continue ;
      } else {
        return List.hd(mergepairs(/* tuple */[
                        param[1],
                        0
                      ]));
      }
    };
  } else {
    return /* [] */0;
  }
}

function remdups(param) {
  if (param) {
    var match = param[1];
    var x = param[0];
    if (match) {
      var x2 = match[0];
      var rest = remdups(/* :: */[
            x2,
            match[1]
          ]);
      if (Caml_obj.caml_equal(x, x2)) {
        return rest;
      } else {
        return /* :: */[
                x,
                rest
              ];
      }
    } else {
      return /* :: */[
              x,
              /* [] */0
            ];
    }
  } else {
    return /* [] */0;
  }
}

function sortunique($less) {
  return (function (param) {
      return Sml$ReasonReactExamples.$less$dot$great(remdups, (function (param) {
                    return sort($less, param);
                  }), param);
    });
}

function earlierlist(a1, _a2, _a3) {
  while(true) {
    var a3 = _a3;
    var a2 = _a2;
    if (a2) {
      if (a3) {
        var y = a3[0];
        var x = a2[0];
        if (Curry._2(a1, x, y)) {
          return true;
        } else if (Curry._2(a1, y, x)) {
          return false;
        } else {
          _a3 = a3[1];
          _a2 = a2[1];
          continue ;
        }
      } else {
        return false;
      }
    } else if (a3) {
      return true;
    } else {
      return false;
    }
  };
}

function sorteddiff($less, _xs, _ys) {
  while(true) {
    var ys = _ys;
    var xs = _xs;
    if (xs) {
      if (ys) {
        var ys$1 = ys[1];
        var y1 = ys[0];
        var xs$1 = xs[1];
        var x1 = xs[0];
        if (Caml_obj.caml_equal(x1, y1)) {
          _ys = ys$1;
          _xs = xs$1;
          continue ;
        } else if (Curry._2($less, x1, y1)) {
          return /* :: */[
                  x1,
                  sorteddiff($less, xs$1, /* :: */[
                        y1,
                        ys$1
                      ])
                ];
        } else {
          _ys = ys$1;
          _xs = /* :: */[
            x1,
            xs$1
          ];
          continue ;
        }
      } else {
        return xs;
      }
    } else {
      return /* [] */0;
    }
  };
}

function sortedsame(a1, _a2, _a3) {
  while(true) {
    var a3 = _a3;
    var a2 = _a2;
    if (a2 && a3) {
      var ys = a3[1];
      var y1 = a3[0];
      var xs = a2[1];
      var x1 = a2[0];
      if (Caml_obj.caml_equal(x1, y1)) {
        return /* :: */[
                x1,
                sortedsame(a1, xs, ys)
              ];
      } else if (Curry._2(a1, x1, y1)) {
        _a3 = /* :: */[
          y1,
          ys
        ];
        _a2 = xs;
        continue ;
      } else {
        _a3 = ys;
        _a2 = /* :: */[
          x1,
          xs
        ];
        continue ;
      }
    } else {
      return /* [] */0;
    }
  };
}

function sortedmergeandcombine($less, $plus, xs, ys) {
  var s = function (a1, a2) {
    if (a1) {
      if (a2) {
        var ys = a2[1];
        var y1 = a2[0];
        var xs = a1[1];
        var x1 = a1[0];
        if (Curry._2($less, x1, y1)) {
          return /* :: */[
                  x1,
                  s(xs, /* :: */[
                        y1,
                        ys
                      ])
                ];
        } else if (Curry._2($less, y1, x1)) {
          return /* :: */[
                  y1,
                  s(/* :: */[
                        x1,
                        xs
                      ], ys)
                ];
        } else {
          return /* :: */[
                  Curry._2($plus, x1, y1),
                  s(xs, ys)
                ];
        }
      } else {
        return a1;
      }
    } else {
      return a2;
    }
  };
  return s(xs, ys);
}

function sortedmerge($less, xs, ys) {
  return sortedmergeandcombine($less, (function (x, param) {
                return x;
              }), xs, ys);
}

function sortedlistsub(eq, xs, ys) {
  var g = function (xs, param) {
    if (param) {
      var ys = param[1];
      var y = param[0];
      var f = function (param) {
        if (param) {
          var xs = param[1];
          var x = param[0];
          if (Curry._1(eq, /* tuple */[
                  x,
                  y
                ])) {
            return g(xs, ys);
          } else {
            return /* :: */[
                    x,
                    f(xs)
                  ];
          }
        } else {
          return /* [] */0;
        }
      };
      return f(xs);
    } else {
      return xs;
    }
  };
  return g(xs, ys);
}

function matchbag(pp, xs) {
  var revapp = function (_a1, _a2) {
    while(true) {
      var a2 = _a2;
      var a1 = _a1;
      if (a1) {
        _a2 = /* :: */[
          a1[0],
          a2
        ];
        _a1 = a1[1];
        continue ;
      } else {
        return a2;
      }
    };
  };
  var _r = /* [] */0;
  var _pre = /* [] */0;
  var _param = xs;
  while(true) {
    var param = _param;
    var pre = _pre;
    var r = _r;
    if (param) {
      var xs$1 = param[1];
      var x = param[0];
      var match = Curry._1(pp, x);
      _param = xs$1;
      _pre = /* :: */[
        x,
        pre
      ];
      if (match !== undefined) {
        _r = /* :: */[
          /* tuple */[
            x,
            Caml_option.valFromOption(match),
            revapp(pre, xs$1)
          ],
          r
        ];
        continue ;
      } else {
        continue ;
      }
    } else {
      return r;
    }
  };
}

function $great$less(xs, ys) {
  return List.concat(List.map((function (x) {
                    return List.map((function (y) {
                                  return /* tuple */[
                                          x,
                                          y
                                        ];
                                }), ys);
                  }), xs));
}

function allpairs(xs) {
  return Sml$ReasonReactExamples.nj_fold((function (param) {
                  var match = param[1];
                  var ys = match[0];
                  var x = param[0];
                  return /* tuple */[
                          /* :: */[
                            x,
                            ys
                          ],
                          Sml$ReasonReactExamples.nj_fold((function (param) {
                                  return /* :: */[
                                          /* tuple */[
                                            x,
                                            param[0]
                                          ],
                                          param[1]
                                        ];
                                }), ys, match[1])
                        ];
                }), xs, /* tuple */[
                /* [] */0,
                /* [] */0
              ])[1];
}

function eqlists(a1, _a2) {
  while(true) {
    var a2 = _a2;
    var match = a2[0];
    if (match) {
      var match$1 = a2[1];
      if (match$1 && Curry._1(a1, /* tuple */[
              match[0],
              match$1[0]
            ])) {
        _a2 = /* tuple */[
          match[1],
          match$1[1]
        ];
        continue ;
      } else {
        return false;
      }
    } else if (a2[1]) {
      return false;
    } else {
      return true;
    }
  };
}

function numbered(xs) {
  var r = function (n, param) {
    if (param) {
      return /* :: */[
              /* tuple */[
                n,
                param[0]
              ],
              r(n + 1 | 0, param[1])
            ];
    } else {
      return /* [] */0;
    }
  };
  return r(0, xs);
}

function listsub(eq, xs, ys) {
  var _a1 = xs;
  var _a2 = ys;
  while(true) {
    var a2 = _a2;
    var a1 = _a1;
    if (a1) {
      if (a2) {
        var y = a2[0];
        var strip = (function(y){
        return function strip(param) {
          if (param) {
            var xs = param[1];
            var x = param[0];
            if (Curry._1(eq, /* tuple */[
                    x,
                    y
                  ])) {
              return xs;
            } else {
              return /* :: */[
                      x,
                      strip(xs)
                    ];
            }
          } else {
            return /* [] */0;
          }
        }
        }(y));
        _a2 = a2[1];
        _a1 = strip(a1);
        continue ;
      } else {
        return a1;
      }
    } else {
      return /* [] */0;
    }
  };
}

function eqbags(ee, param) {
  var ys = param[1];
  var xs = param[0];
  if (List.length(xs) === List.length(ys)) {
    return Sml$ReasonReactExamples.$$null(listsub(ee, xs, ys));
  } else {
    return false;
  }
}

function toposort(roots, depf) {
  var ts = function (visited, param) {
    var match = param[1];
    var cycles = match[1];
    var order = match[0];
    var root = param[0];
    if (member(/* tuple */[
            root,
            visited
          ])) {
      var cycle_001 = List.rev(/* :: */[
            root,
            takewhile((function (x) {
                    return Caml_obj.caml_notequal(x, root);
                  }), visited)
          ]);
      var cycle = /* :: */[
        root,
        cycle_001
      ];
      return /* tuple */[
              order,
              /* :: */[
                cycle,
                cycles
              ]
            ];
    } else if (member(/* tuple */[
            root,
            order
          ])) {
      return /* tuple */[
              order,
              cycles
            ];
    } else {
      var children = Curry._1(depf, root);
      var partial_arg = /* :: */[
        root,
        visited
      ];
      var match$1 = Sml$ReasonReactExamples.nj_revfold((function (param) {
              return ts(partial_arg, param);
            }), children, /* tuple */[
            order,
            cycles
          ]);
      return /* tuple */[
              /* :: */[
                root,
                match$1[0]
              ],
              match$1[1]
            ];
    }
  };
  return Sml$ReasonReactExamples.nj_revfold((function (param) {
                return ts(/* [] */0, param);
              }), roots, /* tuple */[
              /* [] */0,
              /* [] */0
            ]);
}

var $less$pipe = List.filter;

var $less$star = List.map;

exports.First = First;
exports.Reduce = Reduce;
exports.foldr = foldr;
exports.foldl = foldl;
exports.$less$pipe = $less$pipe;
exports.$less$star = $less$star;
exports.Zip_ = Zip_;
exports.$pipe$pipe$pipe = $pipe$pipe$pipe;
exports.doubleslosh = doubleslosh;
exports.$slash$great = $slash$great;
exports.$less$slash = $less$slash;
exports.$slash$slash = $slash$slash;
exports.all = all;
exports.all1 = all1;
exports.member = member;
exports.nonmember = nonmember;
exports.slosh = slosh;
exports.seteq = seteq;
exports.set = set;
exports.subset = subset;
exports._INTER = _INTER;
exports.interpolate = interpolate;
exports.catelim_interpolate = catelim_interpolate;
exports.stringfn_of_catelim = stringfn_of_catelim;
exports.catelim_of_stringfn = catelim_of_stringfn;
exports.catelim_string_of_list = catelim_string_of_list;
exports.catelim_sentencestring_of_list = catelim_sentencestring_of_list;
exports.catelim_bracketedstring_of_list = catelim_bracketedstring_of_list;
exports.string_of_list = string_of_list;
exports.sentencestring_of_list = sentencestring_of_list;
exports.bracketed_string_of_list = bracketed_string_of_list;
exports.replacenth = replacenth;
exports.Bad_nth = Bad_nth;
exports.guardednth = guardednth;
exports.Last_ = Last_;
exports.last = last;
exports.take = take;
exports.drop = drop;
exports.takewhile = takewhile;
exports.dropwhile = dropwhile;
exports._BMzip = _BMzip;
exports.isprefix = isprefix;
exports.Extract_ = Extract_;
exports.extract = extract;
exports.split = split;
exports.Matchinmergepairs = Matchinmergepairs;
exports.sort = sort;
exports.sortandcombine = sortandcombine;
exports.remdups = remdups;
exports.sortunique = sortunique;
exports.earlierlist = earlierlist;
exports.sorteddiff = sorteddiff;
exports.sortedsame = sortedsame;
exports.sortedmergeandcombine = sortedmergeandcombine;
exports.sortedmerge = sortedmerge;
exports.sortedlistsub = sortedlistsub;
exports.matchbag = matchbag;
exports.$great$less = $great$less;
exports.allpairs = allpairs;
exports.eqlists = eqlists;
exports.numbered = numbered;
exports.listsub = listsub;
exports.eqbags = eqbags;
exports.toposort = toposort;
/* No side effect */
