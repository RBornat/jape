'use strict';

var List = require("bs-platform/lib/js/list.js");
var Block = require("bs-platform/lib/js/block.js");
var Curry = require("bs-platform/lib/js/curry.js");
var Caml_obj = require("bs-platform/lib/js/caml_obj.js");
var Caml_exceptions = require("bs-platform/lib/js/caml_exceptions.js");
var Sml$ReasonReactExamples = require("../utils/sml.bs.js");
var Listfuns$ReasonReactExamples = require("../utils/listfuns.bs.js");

function emptysearchtree(f) {
  return /* SearchTree */[/* tuple */[
            /* [] */0,
            /* Unbuilt */Block.__(0, [f])
          ]];
}

function mkalt(param) {
  if (param.tag) {
    return param[0][0];
  } else {
    return param[0];
  }
}

function same(eqr, param, param$1) {
  if (Caml_obj.caml_equal(param[0], param$1[0]) && Curry._1(eqr, /* tuple */[
          param[1],
          param$1[1]
        ])) {
    return Caml_obj.caml_equal(param[2], param$1[2]);
  } else {
    return false;
  }
}

function diff(cs, param) {
  return Caml_obj.caml_notequal(cs, param[0]);
}

function addtotree(eqr, t, info) {
  var cs = info[0];
  var match = t[0];
  var csrbs = match[0];
  if (List.exists((function (param) {
            return same(eqr, info, param);
          }), csrbs)) {
    return t;
  } else {
    return /* SearchTree */[/* tuple */[
              /* :: */[
                info,
                Listfuns$ReasonReactExamples.$less$pipe((function (param) {
                          return diff(cs, param);
                        }))(csrbs)
              ],
              /* Unbuilt */Block.__(0, [mkalt(match[1])])
            ]];
  }
}

var DeleteFromTree_ = Caml_exceptions.create("Searchtree-ReasonReactExamples.DeleteFromTree_");

function deletefromtree(eqr, param, info) {
  var cs = info[0];
  var match = param[0];
  var csrbs = match[0];
  if (List.exists((function (param) {
            return same(eqr, info, param);
          }), csrbs)) {
    return /* SearchTree */[/* tuple */[
              Listfuns$ReasonReactExamples.$less$pipe((function (param) {
                        return diff(cs, param);
                      }))(csrbs),
              /* Unbuilt */Block.__(0, [mkalt(match[1])])
            ]];
  } else {
    throw DeleteFromTree_;
  }
}

function summarisetree(param) {
  return param[0][0];
}

function catelim_string_of_fsm(cf, rf, t, ss) {
  if (typeof t === "number") {
    return /* :: */[
            "Wrong",
            ss
          ];
  } else {
    switch (t.tag | 0) {
      case /* Answer */0 :
          return /* :: */[
                  "Answer(",
                  Curry._2(rf, t[0], /* :: */[
                        ")",
                        ss
                      ])
                ];
      case /* Prefix */1 :
          var match = t[0];
          return /* :: */[
                  "Prefix(",
                  Curry._2(rf, match[0], /* :: */[
                        ",",
                        catelim_string_of_fsm(cf, rf, match[1], /* :: */[
                              ")",
                              ss
                            ])
                      ])
                ];
      case /* Shift */2 :
          return /* :: */[
                  "Shift(",
                  catelim_string_of_fsm(cf, rf, t[0], /* :: */[
                        ")",
                        ss
                      ])
                ];
      case /* Alt */3 :
          return /* :: */[
                  "Alt ...",
                  ss
                ];
      case /* Eq */4 :
          var match$1 = t[0];
          return /* :: */[
                  "Eq(",
                  Curry._2(cf, match$1[0], /* :: */[
                        ",",
                        catelim_string_of_fsm(cf, rf, match$1[1], /* :: */[
                              ",",
                              catelim_string_of_fsm(cf, rf, match$1[2], /* :: */[
                                    ")",
                                    ss
                                  ])
                            ])
                      ])
                ];
      
    }
  }
}

function rootfsm(rt) {
  var match = rt.contents;
  var match$1 = match[0];
  var match$2 = match$1[1];
  var csrbs = match$1[0];
  if (match$2.tag) {
    return match$2[0][1];
  } else {
    var mkalt = match$2[0];
    var doit = function (csrbs) {
      var match = Listfuns$ReasonReactExamples.split((function (param) {
              return Sml$ReasonReactExamples.$$null(param[0]);
            }), csrbs);
      var match$1 = match[0];
      if (match$1) {
        var match$2 = match$1[0];
        var r = match$2[1];
        if (match$2[2]) {
          return /* Prefix */Block.__(1, [/* tuple */[
                      r,
                      doit(match[1])
                    ]]);
        } else {
          var qs = match[1];
          if (qs) {
            return doalt(/* [] */0, qs, /* Answer */Block.__(0, [r]));
          } else {
            return /* Answer */Block.__(0, [r]);
          }
        }
      } else if (match[1]) {
        return doalt(/* [] */0, match[1], /* Wrong */0);
      } else {
        return /* Wrong */0;
      }
    };
    var doalt = function (_a1, _a2, a3) {
      while(true) {
        var a2 = _a2;
        var a1 = _a1;
        if (a2) {
          var c = List.hd(List.hd(a2)[0]);
          var match = Listfuns$ReasonReactExamples.split((function(c){
              return function (param) {
                return Caml_obj.caml_equal(List.hd(param[0]), c);
              }
              }(c)), a2);
          _a2 = match[1];
          _a1 = /* :: */[
            /* tuple */[
              c,
              Listfuns$ReasonReactExamples.$less$star((function (param) {
                      return /* tuple */[
                              List.tl(param[0]),
                              param[1],
                              param[2]
                            ];
                    }), match[0])
            ],
            a1
          ];
          continue ;
        } else {
          var cts = Listfuns$ReasonReactExamples.$less$star((function (param) {
                  return /* tuple */[
                          param[0],
                          doit(param[1])
                        ];
                }), a1);
          if (cts) {
            if (cts[1]) {
              return /* Alt */Block.__(3, [Curry._2(mkalt, Listfuns$ReasonReactExamples.$less$star((function (param) {
                                    return /* tuple */[
                                            param[0],
                                            /* Shift */Block.__(2, [param[1]])
                                          ];
                                  }), cts), a3)]);
            } else {
              var match$1 = cts[0];
              return /* Eq */Block.__(4, [/* tuple */[
                          match$1[0],
                          match$1[1],
                          a3
                        ]]);
            }
          } else {
            return a3;
          }
        }
      };
    };
    var t = doit(csrbs);
    rt.contents = /* SearchTree */[/* tuple */[
        csrbs,
        /* Built */Block.__(1, [/* tuple */[
              mkalt,
              t
            ]])
      ]];
    return t;
  }
}

function fsmpos(_t, _cs) {
  while(true) {
    var cs = _cs;
    var t = _t;
    if (cs) {
      if (typeof t === "number") {
        return ;
      } else {
        switch (t.tag | 0) {
          case /* Answer */0 :
              return ;
          case /* Prefix */1 :
              _t = t[0][1];
              continue ;
          case /* Shift */2 :
              _cs = cs[1];
              _t = t[0];
              continue ;
          case /* Alt */3 :
              _t = Curry._1(t[0], cs[0]);
              continue ;
          case /* Eq */4 :
              var match = t[0];
              if (Caml_obj.caml_equal(cs[0], match[0])) {
                _cs = cs[1];
                _t = match[1];
                continue ;
              } else {
                _t = match[2];
                continue ;
              }
          
        }
      }
    } else {
      return t;
    }
  };
}

function scanfsm(next, t, rcs, c) {
  var scan = function (_t, _rcs, _c) {
    while(true) {
      var c = _c;
      var rcs = _rcs;
      var t = _t;
      if (typeof t === "number") {
        return /* NotFound */Block.__(1, [rcs]);
      } else {
        switch (t.tag | 0) {
          case /* Answer */0 :
              return /* Found */Block.__(0, [/* tuple */[
                          t[0],
                          rcs
                        ]]);
          case /* Prefix */1 :
              var match = t[0];
              var res = scan(match[1], rcs, c);
              if (res.tag) {
                return /* Found */Block.__(0, [/* tuple */[
                            match[0],
                            rcs
                          ]]);
              } else {
                return res;
              }
          case /* Shift */2 :
              _c = Curry._1(next, /* () */0);
              _rcs = /* :: */[
                c,
                rcs
              ];
              _t = t[0];
              continue ;
          case /* Alt */3 :
              _t = Curry._1(t[0], c);
              continue ;
          case /* Eq */4 :
              var match$1 = t[0];
              if (Caml_obj.caml_equal(c, match$1[0])) {
                _c = Curry._1(next, /* () */0);
                _rcs = /* :: */[
                  c,
                  rcs
                ];
                _t = match$1[1];
                continue ;
              } else {
                _t = match$1[2];
                continue ;
              }
          
        }
      }
    };
  };
  return scan(t, rcs, c);
}

function scanstatefsm(curr, move, t, state) {
  var scan = function (_t, _state) {
    while(true) {
      var state = _state;
      var t = _t;
      if (typeof t === "number") {
        return /* NotFound */Block.__(1, [state]);
      } else {
        switch (t.tag | 0) {
          case /* Answer */0 :
              return /* Found */Block.__(0, [/* tuple */[
                          t[0],
                          state
                        ]]);
          case /* Prefix */1 :
              var match = t[0];
              var res = scan(match[1], state);
              if (res.tag) {
                return /* Found */Block.__(0, [/* tuple */[
                            match[0],
                            res[0]
                          ]]);
              } else {
                return res;
              }
          case /* Shift */2 :
              _state = Curry._1(move, state);
              _t = t[0];
              continue ;
          case /* Alt */3 :
              _t = Curry._1(t[0], Curry._1(curr, state));
              continue ;
          case /* Eq */4 :
              var match$1 = t[0];
              if (Caml_obj.caml_equal(Curry._1(curr, state), match$1[0])) {
                _state = Curry._1(move, state);
                _t = match$1[1];
                continue ;
              } else {
                _t = match$1[2];
                continue ;
              }
          
        }
      }
    };
  };
  return scan(t, state);
}

function searchfsm(t, cs) {
  var search = function (_t, _rcs, _cs) {
    while(true) {
      var cs = _cs;
      var rcs = _rcs;
      var t = _t;
      if (typeof t === "number") {
        return /* NotFound */Block.__(1, [rcs]);
      } else {
        switch (t.tag | 0) {
          case /* Answer */0 :
              if (cs) {
                return /* NotFound */Block.__(1, [rcs]);
              } else {
                return /* Found */Block.__(0, [/* tuple */[
                            t[0],
                            rcs
                          ]]);
              }
          case /* Prefix */1 :
              var match = t[0];
              var res = search(match[1], rcs, cs);
              if (res.tag) {
                return /* Found */Block.__(0, [/* tuple */[
                            match[0],
                            rcs
                          ]]);
              } else {
                return res;
              }
          case /* Shift */2 :
              if (cs) {
                _cs = cs[1];
                _rcs = /* :: */[
                  cs[0],
                  rcs
                ];
                _t = t[0];
                continue ;
              } else {
                return /* NotFound */Block.__(1, [rcs]);
              }
          case /* Alt */3 :
              if (cs) {
                _t = Curry._1(t[0], cs[0]);
                continue ;
              } else {
                return /* NotFound */Block.__(1, [rcs]);
              }
          case /* Eq */4 :
              if (cs) {
                var c = cs[0];
                var match$1 = t[0];
                if (Caml_obj.caml_equal(c, match$1[0])) {
                  _cs = cs[1];
                  _rcs = /* :: */[
                    c,
                    rcs
                  ];
                  _t = match$1[1];
                  continue ;
                } else {
                  _t = match$1[2];
                  continue ;
                }
              } else {
                return /* NotFound */Block.__(1, [rcs]);
              }
          
        }
      }
    };
  };
  return search(t, /* [] */0, cs);
}

exports.emptysearchtree = emptysearchtree;
exports.mkalt = mkalt;
exports.same = same;
exports.diff = diff;
exports.addtotree = addtotree;
exports.DeleteFromTree_ = DeleteFromTree_;
exports.deletefromtree = deletefromtree;
exports.summarisetree = summarisetree;
exports.catelim_string_of_fsm = catelim_string_of_fsm;
exports.rootfsm = rootfsm;
exports.fsmpos = fsmpos;
exports.scanfsm = scanfsm;
exports.scanstatefsm = scanstatefsm;
exports.searchfsm = searchfsm;
/* No side effect */
