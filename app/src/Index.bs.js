'use strict';

var React = require("react");
var ReactDom = require("react-dom");
var App$ReasonReactExamples = require("./App.bs.js");
var ExampleStyles$ReasonReactExamples = require("./ExampleStyles.bs.js");

var style = document.createElement("style");

document.head.appendChild(style);

style.innerHTML = ExampleStyles$ReasonReactExamples.style;

function makeContainer(text) {
  var container = document.createElement("div");
  container.className = "container";
  var title = document.createElement("div");
  title.className = "containerTitle";
  title.innerText = text;
  var content = document.createElement("div");
  content.className = "containerContent";
  container.appendChild(title);
  container.appendChild(content);
  document.body.appendChild(container);
  return content;
}

ReactDom.render(React.createElement(App$ReasonReactExamples.app, { }), makeContainer("Jape theorem prover"));

exports.style = style;
exports.makeContainer = makeContainer;
/* style Not a pure module */
