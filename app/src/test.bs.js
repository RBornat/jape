'use strict';


function f(x) {
  return x + 2 | 0;
}

exports.f = f;
/* No side effect */
