'use strict';

var Curry = require("bs-platform/lib/js/curry.js");
var React = require("react");

function App$app(Props) {
  var match = React.useState((function () {
          return "";
        }));
  var setCurrentTheorem = match[1];
  return React.createElement("div", {
              className: "app"
            }, React.createElement("div", {
                  className: "theoremBrowser"
                }, React.createElement("h4", undefined, "My theorems"), React.createElement("div", {
                      className: "file"
                    }, React.createElement("p", {
                          className: "fileName"
                        }, "Some_file.j"))), React.createElement("div", {
                  className: "theoremEditor"
                }, React.createElement("textarea", {
                      value: match[0],
                      onChange: (function (e) {
                          return Curry._1(setCurrentTheorem, e.target.value);
                        })
                    }), React.createElement("input", {
                      className: "proveTheorem",
                      type: "submit",
                      value: "Prove me ->"
                    })));
}

var app = App$app;

exports.app = app;
/* react Not a pure module */
