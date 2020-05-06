let reasonReactBlue = "#48a9dc";

// The {j|...|j} feature is just string interpolation, from
// bucklescript.github.io/docs/en/interop-cheatsheet#string-unicode-interpolation
// This allows us to conveniently write CSS, together with variables, by
// constructing a string
let style = {j|
  html, body {
    height: 100%;
  }
  body {
    margin: 2em;
  }
  div.containerTitle {
    font-size: 2em;
  }
  div.app {
    display: flex;
    flex-direction: "row";
    margin-top: 15px;
  }
  div.theoremBrowser {
    flex: 1;
    background-color: orange;
    padding: 10px;
    font-size: 1em;
    margin-right: 10px;
    border-radius: 3px;
  }
  div.theoremEditor {
    flex: 4;
  }
  textarea {
    width: 100%;
    height: 100%;
    border: 1px solid black;
    border-radius: 5px;
  }
|j};
