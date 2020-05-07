'use strict';


function int_of_resnum(param) {
  if (typeof param === "number") {
    return 0;
  } else {
    return param[0];
  }
}

function debracket(_t) {
  while(true) {
    var t = _t;
    if (t.tag === /* Fixapp */5) {
      var match = t[0];
      var match$1 = match[1];
      if (match$1 && match$1[0] === "(") {
        var match$2 = match$1[1];
        if (match$2 && match$2[0] === ")" && !match$2[1]) {
          var match$3 = match[2];
          if (match$3 && !match$3[1]) {
            _t = match$3[0];
            continue ;
          } else {
            return t;
          }
        } else {
          return t;
        }
      } else {
        return t;
      }
    } else {
      return t;
    }
  };
}

function bracketed(t) {
  if (t.tag === /* Fixapp */5) {
    var match = t[0];
    var match$1 = match[1];
    if (match$1 && match$1[0] === "(") {
      var match$2 = match$1[1];
      if (match$2 && match$2[0] === ")" && !match$2[1]) {
        var match$3 = match[2];
        if (match$3 && !match$3[1]) {
          return true;
        } else {
          return false;
        }
      } else {
        return false;
      }
    } else {
      return false;
    }
  } else {
    return false;
  }
}

function vid_of_string(s) {
  return s;
}

function string_of_vid(v) {
  return v;
}

exports.int_of_resnum = int_of_resnum;
exports.debracket = debracket;
exports.bracketed = bracketed;
exports.vid_of_string = vid_of_string;
exports.string_of_vid = string_of_vid;
/* No side effect */
