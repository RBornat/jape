'use strict';

var Char = require("bs-platform/lib/js/char.js");
var List = require("bs-platform/lib/js/list.js");
var Curry = require("bs-platform/lib/js/curry.js");
var Stream = require("bs-platform/lib/js/stream.js");
var $$String = require("bs-platform/lib/js/string.js");
var Hashtbl = require("bs-platform/lib/js/hashtbl.js");
var Caml_array = require("bs-platform/lib/js/caml_array.js");
var Pervasives = require("bs-platform/lib/js/pervasives.js");
var Caml_option = require("bs-platform/lib/js/caml_option.js");
var Caml_string = require("bs-platform/lib/js/caml_string.js");
var Caml_exceptions = require("bs-platform/lib/js/caml_exceptions.js");
var Caml_js_exceptions = require("bs-platform/lib/js/caml_js_exceptions.js");
var Caml_builtin_exceptions = require("bs-platform/lib/js/caml_builtin_exceptions.js");
var Sml$ReasonReactExamples = require("./sml.bs.js");
var Listfuns$ReasonReactExamples = require("./listfuns.bs.js");
var Stringfuns$ReasonReactExamples = require("./stringfuns.bs.js");
var Miscellaneous$ReasonReactExamples = require("../miscellaneous.bs.js");

function hexstring_of_char(c) {
  return Stringfuns$ReasonReactExamples.enCharQuote("x" + Stringfuns$ReasonReactExamples.fixedwidth_hexstring_of_int(2, c));
}

function hex4(n) {
  return "0x" + Stringfuns$ReasonReactExamples.fixedwidth_hexstring_of_int(4, n);
}

function hex8(n) {
  return "0x" + Stringfuns$ReasonReactExamples.fixedwidth_hexstring_of_int(8, n);
}

var MalformedUTF_ = Caml_exceptions.create("UTF-ReasonReactExamples.MalformedUTF_");

function check_808f(c) {
  if (c > 143 || c < 128) {
    throw [
          MalformedUTF_,
          /* :: */[
            hexstring_of_char(c),
            /* [] */0
          ]
        ];
  }
  return c & 63;
}

function check_809f(c) {
  if (c > 159 || c < 128) {
    throw [
          MalformedUTF_,
          /* :: */[
            hexstring_of_char(c),
            /* [] */0
          ]
        ];
  }
  return c & 63;
}

function check_80bf(c) {
  if (c > 191 || c < 128) {
    throw [
          MalformedUTF_,
          /* :: */[
            hexstring_of_char(c),
            /* [] */0
          ]
        ];
  }
  return c & 63;
}

function check_90bf(c) {
  if (c > 191 || c < 144) {
    throw [
          MalformedUTF_,
          /* :: */[
            hexstring_of_char(c),
            /* [] */0
          ]
        ];
  }
  return c & 63;
}

function check_a0bf(c) {
  if (c > 191 || c < 160) {
    throw [
          MalformedUTF_,
          /* :: */[
            hexstring_of_char(c),
            /* [] */0
          ]
        ];
  }
  return c & 63;
}

function utf8_next(next) {
  try {
    var n0 = Curry._1(next, 0);
    try {
      var threebyte = function (f) {
        var n1 = Curry._1(f, Curry._1(next, 1));
        try {
          var n2 = check_80bf(Curry._1(next, 2));
          return ((((n0 & 15) << 6) | n1) << 6) | n2;
        }
        catch (raw_exn){
          var exn = Caml_js_exceptions.internalToOCamlException(raw_exn);
          if (exn === Stream.Failure) {
            throw [
                  MalformedUTF_,
                  /* :: */[
                    hexstring_of_char(Char.chr(n1)),
                    /* :: */[
                      " EOF",
                      /* [] */0
                    ]
                  ]
                ];
          }
          if (exn[0] === MalformedUTF_) {
            throw [
                  MalformedUTF_,
                  /* :: */[
                    hexstring_of_char(Char.chr(n1)),
                    /* :: */[
                      " ",
                      exn[1]
                    ]
                  ]
                ];
          }
          throw exn;
        }
      };
      var fourbyte = function (f) {
        var n1 = Curry._1(f, Curry._1(next, 1));
        try {
          var n2 = check_80bf(Curry._1(next, 2));
          try {
            var n3 = check_80bf(Curry._1(next, 3));
            return ((((((n0 & 7) << 6) | n1) << 6) | n2) << 6) | n3;
          }
          catch (raw_exn){
            var exn = Caml_js_exceptions.internalToOCamlException(raw_exn);
            if (exn[0] === MalformedUTF_) {
              throw [
                    MalformedUTF_,
                    /* :: */[
                      hexstring_of_char(Char.chr(n2)),
                      /* :: */[
                        " ",
                        exn[1]
                      ]
                    ]
                  ];
            }
            throw exn;
          }
        }
        catch (raw_exn$1){
          var exn$1 = Caml_js_exceptions.internalToOCamlException(raw_exn$1);
          if (exn$1 === Stream.Failure) {
            throw [
                  MalformedUTF_,
                  /* :: */[
                    hexstring_of_char(Char.chr(n1)),
                    /* :: */[
                      " EOF",
                      /* [] */0
                    ]
                  ]
                ];
          }
          if (exn$1[0] === MalformedUTF_) {
            throw [
                  MalformedUTF_,
                  /* :: */[
                    hexstring_of_char(Char.chr(n1)),
                    /* :: */[
                      " ",
                      exn$1[1]
                    ]
                  ]
                ];
          }
          throw exn$1;
        }
      };
      if (0 <= n0 && n0 <= 127) {
        return n0;
      } else if (194 <= n0 && n0 <= 223) {
        var f = check_80bf;
        var n1 = Curry._1(f, Curry._1(next, 1));
        return ((n0 & 31) << 6) | n1;
      } else if (n0 !== 224) {
        if (225 <= n0 && n0 <= 236 || 238 <= n0 && n0 <= 239) {
          return threebyte(check_80bf);
        } else if (n0 !== 237) {
          if (n0 !== 240) {
            if (241 <= n0 && n0 <= 243) {
              return fourbyte(check_80bf);
            } else {
              if (n0 !== 244) {
                throw [
                      MalformedUTF_,
                      /* [] */0
                    ];
              }
              return fourbyte(check_808f);
            }
          } else {
            return fourbyte(check_90bf);
          }
        } else {
          return threebyte(check_809f);
        }
      } else {
        return threebyte(check_a0bf);
      }
    }
    catch (raw_exn){
      var exn = Caml_js_exceptions.internalToOCamlException(raw_exn);
      if (exn === Stream.Failure) {
        throw [
              MalformedUTF_,
              /* :: */[
                hexstring_of_char(Char.chr(n0)),
                /* :: */[
                  " EOF",
                  /* [] */0
                ]
              ]
            ];
      }
      if (exn[0] === MalformedUTF_) {
        throw [
              MalformedUTF_,
              /* :: */[
                hexstring_of_char(Char.chr(n0)),
                /* :: */[
                  " ",
                  exn[1]
                ]
              ]
            ];
      }
      throw exn;
    }
  }
  catch (raw_exn$1){
    var exn$1 = Caml_js_exceptions.internalToOCamlException(raw_exn$1);
    if (exn$1[0] === MalformedUTF_) {
      throw [
            MalformedUTF_,
            /* :: */[
              "UTF8 ",
              exn$1[1]
            ]
          ];
    }
    throw exn$1;
  }
}

function next16_be(s) {
  var n0 = Stream.next(s);
  try {
    var n1 = Stream.next(s);
    return (n0 << 8) | n1;
  }
  catch (exn){
    if (exn === Stream.Failure) {
      throw [
            MalformedUTF_,
            /* :: */[
              hexstring_of_char(Char.chr(n0)),
              /* :: */[
                " EOF",
                /* [] */0
              ]
            ]
          ];
    }
    throw exn;
  }
}

function next16_le(s) {
  var n1 = Stream.next(s);
  try {
    var n0 = Stream.next(s);
    return (n0 << 8) | n1;
  }
  catch (exn){
    if (exn === Stream.Failure) {
      throw [
            MalformedUTF_,
            /* :: */[
              hexstring_of_char(Char.chr(n1)),
              /* :: */[
                " EOF",
                /* [] */0
              ]
            ]
          ];
    }
    throw exn;
  }
}

function next32_be(s) {
  var n0 = next16_be(s);
  try {
    var n1 = next16_be(s);
    return (n0 << 16) | n1;
  }
  catch (raw_exn){
    var exn = Caml_js_exceptions.internalToOCamlException(raw_exn);
    if (exn === Stream.Failure) {
      throw [
            MalformedUTF_,
            /* :: */[
              hex4(n0),
              /* :: */[
                " EOF",
                /* [] */0
              ]
            ]
          ];
    }
    if (exn[0] === MalformedUTF_) {
      throw [
            MalformedUTF_,
            /* :: */[
              hex4(n0),
              /* :: */[
                " ",
                exn[1]
              ]
            ]
          ];
    }
    throw exn;
  }
}

function next32_le(s) {
  var n1 = next16_le(s);
  try {
    var n0 = next16_le(s);
    return (n0 << 16) | n1;
  }
  catch (raw_exn){
    var exn = Caml_js_exceptions.internalToOCamlException(raw_exn);
    if (exn === Stream.Failure) {
      throw [
            MalformedUTF_,
            /* :: */[
              hex4(n1),
              /* :: */[
                " EOF",
                /* [] */0
              ]
            ]
          ];
    }
    if (exn[0] === MalformedUTF_) {
      throw [
            MalformedUTF_,
            /* :: */[
              hex4(n1),
              /* :: */[
                " ",
                exn[1]
              ]
            ]
          ];
    }
    throw exn;
  }
}

function utf8_get(s, i) {
  if (i === s.length) {
    return -1;
  } else {
    return utf8_next((function (j) {
                  try {
                    return Caml_string.get(s, i + j | 0);
                  }
                  catch (exn){
                    throw Stream.Failure;
                  }
                }));
  }
}

function utf8_next$1(s) {
  return utf8_next((function (param) {
                return Stream.next(s);
              }));
}

function utf16_next(bigendian) {
  var partial_arg = bigendian ? next16_be : next16_le;
  return (function (param) {
      var next16 = partial_arg;
      var s = param;
      try {
        var m = Curry._1(next16, s);
        if (0 <= m && m <= 55295) {
          return m;
        } else if (55296 <= m && m <= 56319) {
          try {
            var m2 = Curry._1(next16, s);
            if (56320 <= m2 && m2 <= 57343) {
              var m$1 = (((m & 1023) << 10) | m2 & 1023) + 65536 | 0;
              if (0 <= m$1 && m$1 <= 55295 || 57344 <= m$1 && m$1 <= 1114111) {
                return m$1;
              } else {
                throw [
                      MalformedUTF_,
                      /* :: */[
                        hex4(m),
                        /* :: */[
                          " ",
                          /* :: */[
                            hex4(m2),
                            /* :: */[
                              " => ",
                              /* :: */[
                                hex8(m$1),
                                /* :: */[
                                  " -- no such code point",
                                  /* [] */0
                                ]
                              ]
                            ]
                          ]
                        ]
                      ]
                    ];
              }
            } else {
              throw [
                    MalformedUTF_,
                    /* :: */[
                      hex4(m),
                      /* :: */[
                        " ",
                        /* :: */[
                          hex4(m2),
                          /* [] */0
                        ]
                      ]
                    ]
                  ];
            }
          }
          catch (exn){
            if (exn === Stream.Failure) {
              throw [
                    MalformedUTF_,
                    /* :: */[
                      hex4(m),
                      /* :: */[
                        " EOF",
                        /* [] */0
                      ]
                    ]
                  ];
            }
            throw exn;
          }
        } else {
          throw [
                MalformedUTF_,
                /* :: */[
                  hex4(m),
                  /* [] */0
                ]
              ];
        }
      }
      catch (raw_exn){
        var exn$1 = Caml_js_exceptions.internalToOCamlException(raw_exn);
        if (exn$1[0] === MalformedUTF_) {
          throw [
                MalformedUTF_,
                /* :: */[
                  "UTF16 ",
                  exn$1[1]
                ]
              ];
        }
        throw exn$1;
      }
    });
}

function utf32_next(bigendian) {
  var partial_arg = bigendian ? next32_be : next32_le;
  return (function (param) {
      var next32 = partial_arg;
      var s = param;
      try {
        var m = Curry._1(next32, s);
        if (0 <= m && m <= 55295 || 57344 <= m && true) {
          return m;
        } else {
          throw [
                MalformedUTF_,
                /* :: */[
                  "UTF32 ",
                  /* :: */[
                    hex8(m),
                    /* :: */[
                      " -- no such code point",
                      /* [] */0
                    ]
                  ]
                ]
              ];
        }
      }
      catch (raw_exn){
        var exn = Caml_js_exceptions.internalToOCamlException(raw_exn);
        if (exn[0] === MalformedUTF_) {
          throw [
                MalformedUTF_,
                /* :: */[
                  "UTF32 ",
                  exn[1]
                ]
              ];
        }
        throw exn;
      }
    });
}

function of_utfstream(reader, s) {
  return Stream.from((function (param) {
                try {
                  return Caml_option.some(Curry._1(reader, s));
                }
                catch (exn){
                  if (exn === Stream.Failure) {
                    return ;
                  } else {
                    throw exn;
                  }
                }
              }));
}

var utf_stdin = of_utfstream(utf8_next$1, Stream.of_channel(Pervasives.stdin));

function njunk(_n, s) {
  while(true) {
    var n = _n;
    if (n > 0) {
      Stream.junk(s);
      _n = n - 1 | 0;
      continue ;
    } else {
      return 0;
    }
  };
}

function of_utfchannel(ic) {
  var s = Stream.of_channel(ic);
  var cs = Stream.npeek(4, s);
  var reader;
  var exit = 0;
  if (cs) {
    var match = cs[0];
    if (match !== 0) {
      if (match >= 255) {
        var match$1 = cs[1];
        if (match$1 && match$1[0] === 254) {
          var match$2 = match$1[1];
          if (match$2 && match$2[0] === 0) {
            var match$3 = match$2[1];
            if (match$3 && !(match$3[0] !== 0 || match$3[1])) {
              njunk(4, s);
              reader = utf32_next(false);
            } else {
              exit = 1;
            }
          } else {
            exit = 1;
          }
        } else {
          exit = 1;
        }
      } else {
        exit = 1;
      }
    } else {
      var match$4 = cs[1];
      if (match$4 && match$4[0] === 0) {
        var match$5 = match$4[1];
        if (match$5 && match$5[0] === 254) {
          var match$6 = match$5[1];
          if (match$6 && match$6[0] >= 255 && !match$6[1]) {
            njunk(4, s);
            reader = utf32_next(true);
          } else {
            exit = 1;
          }
        } else {
          exit = 1;
        }
      } else {
        exit = 1;
      }
    }
  } else {
    exit = 1;
  }
  if (exit === 1) {
    var match$7 = Listfuns$ReasonReactExamples.take(3, cs);
    var exit$1 = 0;
    if (match$7 && match$7[0] === 239) {
      var match$8 = match$7[1];
      if (match$8 && match$8[0] === 187) {
        var match$9 = match$8[1];
        if (match$9 && !(match$9[0] !== 191 || match$9[1])) {
          njunk(3, s);
          reader = utf8_next$1;
        } else {
          exit$1 = 2;
        }
      } else {
        exit$1 = 2;
      }
    } else {
      exit$1 = 2;
    }
    if (exit$1 === 2) {
      var match$10 = Listfuns$ReasonReactExamples.take(2, cs);
      if (match$10) {
        var match$11 = match$10[0];
        if (match$11 !== 254) {
          if (match$11 >= 255) {
            var match$12 = match$10[1];
            if (match$12 && !(match$12[0] !== 254 || match$12[1])) {
              njunk(2, s);
              reader = utf16_next(false);
            } else {
              reader = utf8_next$1;
            }
          } else {
            reader = utf8_next$1;
          }
        } else {
          var match$13 = match$10[1];
          if (match$13 && match$13[0] >= 255 && !match$13[1]) {
            njunk(2, s);
            reader = utf16_next(true);
          } else {
            reader = utf8_next$1;
          }
        }
      } else {
        reader = utf8_next$1;
      }
    }
    
  }
  return of_utfstream(reader, s);
}

function stream_of_utfNinchannel(size, bigendian, ic) {
  var s = Stream.of_channel(ic);
  if (size !== 8) {
    if (size !== 16) {
      if (size !== 32) {
        throw [
              Miscellaneous$ReasonReactExamples.Catastrophe_,
              /* :: */[
                "utf8_of_utfNchannel ",
                /* :: */[
                  String(size),
                  /* [] */0
                ]
              ]
            ];
      }
      var match = Stream.npeek(4, s);
      if (bigendian) {
        if (match && match[0] === 0) {
          var match$1 = match[1];
          if (match$1 && match$1[0] === 0) {
            var match$2 = match$1[1];
            if (match$2 && match$2[0] === 254) {
              var match$3 = match$2[1];
              if (match$3) {
                if (match$3[0] >= 255 && !match$3[1]) {
                  njunk(4, s);
                }
                
              }
              
            }
            
          }
          
        }
        
      } else if (match && match[0] >= 255) {
        var match$4 = match[1];
        if (match$4 && match$4[0] === 254) {
          var match$5 = match$4[1];
          if (match$5 && match$5[0] === 0) {
            var match$6 = match$5[1];
            if (match$6) {
              if (match$6[0] !== 0 || match$6[1]) {
                
              } else {
                njunk(4, s);
              }
            }
            
          }
          
        }
        
      }
      return of_utfstream(utf32_next(bigendian), s);
    } else {
      var match$7 = Stream.npeek(2, s);
      if (bigendian) {
        if (match$7 && match$7[0] === 254) {
          var match$8 = match$7[1];
          if (match$8) {
            if (match$8[0] >= 255 && !match$8[1]) {
              njunk(2, s);
            }
            
          }
          
        }
        
      } else if (match$7 && match$7[0] >= 255) {
        var match$9 = match$7[1];
        if (match$9) {
          if (match$9[0] !== 254 || match$9[1]) {
            
          } else {
            njunk(2, s);
          }
        }
        
      }
      return of_utfstream(utf16_next(bigendian), s);
    }
  } else {
    var match$10 = Stream.npeek(3, s);
    if (match$10 && match$10[0] === 239) {
      var match$11 = match$10[1];
      if (match$11 && match$11[0] === 187) {
        var match$12 = match$11[1];
        if (match$12) {
          if (match$12[0] !== 191 || match$12[1]) {
            
          } else {
            njunk(3, s);
          }
        }
        
      }
      
    }
    return of_utfstream(utf8_next$1, s);
  }
}

function stream_of_utf8string(s) {
  return of_utfstream(utf8_next$1, Stream.of_string(s));
}

function open_out_utf8(s) {
  var ic = Pervasives.open_out(s);
  Pervasives.output_string(ic, Miscellaneous$ReasonReactExamples.utf8BOM);
  return ic;
}

var utf8_widths = Caml_array.caml_make_vect(256, 0);

for(var i = 0; i <= 127; ++i){
  Caml_array.caml_array_set(utf8_widths, i, 1);
}

for(var i$1 = 194; i$1 <= 223; ++i$1){
  Caml_array.caml_array_set(utf8_widths, i$1, 2);
}

for(var i$2 = 224; i$2 <= 239; ++i$2){
  Caml_array.caml_array_set(utf8_widths, i$2, 3);
}

for(var i$3 = 240; i$3 <= 244; ++i$3){
  Caml_array.caml_array_set(utf8_widths, i$3, 4);
}

function utf8width_from_header(c) {
  return Caml_array.caml_array_get(utf8_widths, c);
}

function utf8_explode(s) {
  var lim = s.length;
  var f = function (i) {
    if (i === lim) {
      return /* [] */0;
    } else {
      var n = Caml_array.caml_array_get(utf8_widths, Caml_string.get(s, i));
      return /* :: */[
              utf8_get(s, i),
              f(i + n | 0)
            ];
    }
  };
  return f(0);
}

function utf8_sub(s, i) {
  if (i === s.length) {
    return -1;
  } else {
    return utf8_get(s, i);
  }
}

function utf8_presub(s, i) {
  if (i === 0) {
    return -1;
  } else {
    var _j = i - 1 | 0;
    while(true) {
      var j = _j;
      if (j < 0) {
        throw [
              MalformedUTF_,
              /* :: */[
                "UTF8 string ",
                /* :: */[
                  Listfuns$ReasonReactExamples.bracketed_string_of_list(hexstring_of_char, "; ")(Sml$ReasonReactExamples.chars_of_string($$String.sub(s, 0, i))),
                  /* [] */0
                ]
              ]
            ];
      }
      var n = Caml_array.caml_array_get(utf8_widths, Caml_string.get(s, j));
      if (n > 0) {
        return utf8_get(s, j);
      } else {
        _j = j - 1 | 0;
        continue ;
      }
    };
  }
}

function utf8_of_ucode(m, cs) {
  if (m === -1) {
    return cs;
  } else if (0 <= m && m <= 127) {
    return /* :: */[
            Char.chr(m),
            cs
          ];
  } else if (128 <= m && m <= 2047) {
    var y = (m >>> 6);
    var x = m & 63;
    return /* :: */[
            Char.chr(y | 192),
            /* :: */[
              Char.chr(x | 128),
              cs
            ]
          ];
  } else if (2048 <= m && m <= 57343 || 917504 <= m && false) {
    var x$1 = m & 63;
    var y$1 = (m >>> 6) & 63;
    var z = (m >>> 12);
    return /* :: */[
            Char.chr(z | 224),
            /* :: */[
              Char.chr(y$1 | 128),
              /* :: */[
                Char.chr(x$1 | 128),
                cs
              ]
            ]
          ];
  } else if (65536 <= m && m <= 1114111) {
    var x$2 = m & 63;
    var y$2 = (m >>> 6) & 63;
    var z$1 = (m >>> 12) & 63;
    var u = (m >>> 18);
    return /* :: */[
            Char.chr(u | 240),
            /* :: */[
              Char.chr(z$1 | 128),
              /* :: */[
                Char.chr(y$2 | 128),
                /* :: */[
                  Char.chr(x$2 | 128),
                  cs
                ]
              ]
            ]
          ];
  } else {
    throw [
          MalformedUTF_,
          /* :: */[
            "int ",
            /* :: */[
              hex8(m),
              /* :: */[
                " -- no such code point",
                /* [] */0
              ]
            ]
          ]
        ];
  }
}

function utf8_implode(ns) {
  return Sml$ReasonReactExamples.string_of_chars(List.fold_right(utf8_of_ucode, ns, /* [] */0));
}

function utf8_of_ucode$1(m) {
  return Sml$ReasonReactExamples.string_of_chars(utf8_of_ucode(m, /* [] */0));
}

function utf8width_from_ucode(m) {
  if (0 <= m && m <= 127) {
    return 1;
  } else if (128 <= m && m <= 2047) {
    return 2;
  } else if (2048 <= m && m <= 57343 || 917504 <= m && false) {
    return 3;
  } else if (65536 <= m && m <= 1114111) {
    return 4;
  } else {
    throw [
          MalformedUTF_,
          /* :: */[
            "utf8_width_from_ucode ",
            /* :: */[
              hex8(m),
              /* :: */[
                " -- no such code point",
                /* [] */0
              ]
            ]
          ]
        ];
  }
}

var utf8LSQUOTE = utf8_of_ucode$1(8216);

var utf8RSQUOTE = utf8_of_ucode$1(8217);

function words(s) {
  if (s === "") {
    return /* [] */0;
  } else {
    var wds = function (_param) {
      while(true) {
        var param = _param;
        if (param) {
          var c = param[0];
          switch (c) {
            case 32 :
                var cs = param[1];
                if (cs) {
                  if (cs[0] !== 32) {
                    return /* :: */[
                            /* [] */0,
                            wds(cs)
                          ];
                  } else {
                    _param = /* :: */[
                      32,
                      cs[1]
                    ];
                    continue ;
                  }
                } else {
                  return /* :: */[
                          /* [] */0,
                          /* [] */0
                        ];
                }
            case 33 :
                break;
            case 34 :
                var ws = qds(param[1]);
                return /* :: */[
                        /* :: */[
                          34,
                          List.hd(ws)
                        ],
                        List.tl(ws)
                      ];
            default:
              
          }
          var ws$1 = wds(param[1]);
          return /* :: */[
                  /* :: */[
                    c,
                    List.hd(ws$1)
                  ],
                  List.tl(ws$1)
                ];
        } else {
          return /* :: */[
                  /* [] */0,
                  /* [] */0
                ];
        }
      };
    };
    var qds = function (_param) {
      while(true) {
        var param = _param;
        if (param) {
          var c = param[0];
          if (c !== 34) {
            var ws = qds(param[1]);
            return /* :: */[
                    /* :: */[
                      c,
                      List.hd(ws)
                    ],
                    List.tl(ws)
                  ];
          } else {
            var cs = param[1];
            if (cs) {
              if (cs[0] !== 32) {
                return /* :: */[
                        /* :: */[
                          34,
                          /* [] */0
                        ],
                        wds(cs)
                      ];
              } else {
                _param = /* :: */[
                  34,
                  cs[1]
                ];
                continue ;
              }
            } else {
              return /* :: */[
                      /* :: */[
                        34,
                        /* [] */0
                      ],
                      /* [] */0
                    ];
            }
          }
        } else {
          return /* :: */[
                  /* [] */0,
                  /* [] */0
                ];
        }
      };
    };
    return List.map(utf8_implode, wds(utf8_explode(s)));
  }
}

function respace(ws) {
  return $$String.concat(" ", ws);
}

function charpred(s) {
  var v = Hashtbl.create(undefined, (s.length << 1));
  $$String.iter((function (c) {
          return Hashtbl.add(v, c, true);
        }), s);
  return /* tuple */[
          (function (c) {
              try {
                return Hashtbl.find(v, c);
              }
              catch (exn){
                if (exn === Caml_builtin_exceptions.not_found) {
                  return false;
                } else {
                  throw exn;
                }
              }
            }),
          (function (param) {
              return Hashtbl.add(v, param[0], param[1]);
            })
        ];
}

var match = charpred("abcdefghijklmnopqrstuvwxyz");

var islcletter = match[0];

var match$1 = charpred("ABCDEFGHIJKLMNOPQRSTUVWXYZ");

var isucletter = match$1[0];

function isletter(c) {
  if (Curry._1(islcletter, c)) {
    return true;
  } else {
    return Curry._1(isucletter, c);
  }
}

var match$2 = charpred("0123456789");

var uEOF = -1;

var isdigit = match$2[0];

exports.uEOF = uEOF;
exports.hexstring_of_char = hexstring_of_char;
exports.hex4 = hex4;
exports.hex8 = hex8;
exports.MalformedUTF_ = MalformedUTF_;
exports.check_808f = check_808f;
exports.check_809f = check_809f;
exports.check_80bf = check_80bf;
exports.check_90bf = check_90bf;
exports.check_a0bf = check_a0bf;
exports.next16_be = next16_be;
exports.next16_le = next16_le;
exports.next32_be = next32_be;
exports.next32_le = next32_le;
exports.utf8_get = utf8_get;
exports.utf8_next = utf8_next$1;
exports.utf16_next = utf16_next;
exports.utf32_next = utf32_next;
exports.of_utfstream = of_utfstream;
exports.utf_stdin = utf_stdin;
exports.njunk = njunk;
exports.of_utfchannel = of_utfchannel;
exports.stream_of_utfNinchannel = stream_of_utfNinchannel;
exports.stream_of_utf8string = stream_of_utf8string;
exports.open_out_utf8 = open_out_utf8;
exports.utf8_widths = utf8_widths;
exports.utf8width_from_header = utf8width_from_header;
exports.utf8_explode = utf8_explode;
exports.utf8_sub = utf8_sub;
exports.utf8_presub = utf8_presub;
exports.utf8_implode = utf8_implode;
exports.utf8_of_ucode = utf8_of_ucode$1;
exports.utf8width_from_ucode = utf8width_from_ucode;
exports.utf8LSQUOTE = utf8LSQUOTE;
exports.utf8RSQUOTE = utf8RSQUOTE;
exports.words = words;
exports.respace = respace;
exports.charpred = charpred;
exports.islcletter = islcletter;
exports.isucletter = isucletter;
exports.isletter = isletter;
exports.isdigit = isdigit;
/* utf_stdin Not a pure module */
