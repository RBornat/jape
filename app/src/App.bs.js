'use strict';

var React = require("react");

function App$app(Props) {
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
                }, React.createElement("textarea", undefined)));
}

var app = App$app;

exports.app = app;
/* react Not a pure module */
