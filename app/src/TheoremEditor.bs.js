'use strict';

var List = require("bs-platform/lib/js/list.js");
var $$Array = require("bs-platform/lib/js/array.js");
var Curry = require("bs-platform/lib/js/curry.js");
var React = require("react");

function TheoremEditor$theoremEditor(Props) {
  var match = React.useState((function () {
          return /* [] */0;
        }));
  var setCurrentTheorem = match[1];
  return React.createElement("div", {
              className: "theoremEditor"
            }, $$Array.of_list(List.map((function (param) {
                        return React.createElement("input", undefined);
                      }), match[0])), React.createElement("button", {
                  className: "newLineButton",
                  onClick: (function (param) {
                      return Curry._1(setCurrentTheorem, (function (old) {
                                    return /* :: */[
                                            "",
                                            old
                                          ];
                                  }));
                    })
                }));
}

var theoremEditor = TheoremEditor$theoremEditor;

exports.theoremEditor = theoremEditor;
/* react Not a pure module */
