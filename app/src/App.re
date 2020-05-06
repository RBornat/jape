[@react.component]
let app = () => {
  let (currentTheorem, setCurrentTheorem) = React.useState(() => "");
  <div className="app">
    <div className="theoremBrowser">
      <h4> {React.string("My theorems")} </h4>
      <div className="file">
        <p className="fileName"> {React.string("Some_file.j")} </p>
      </div>
    </div>
    <div className="theoremEditor">
      <textarea
        value=currentTheorem
        onChange={e => setCurrentTheorem(ReactEvent.Form.target(e)##value)}
      />
      <input type_="submit" value="Prove me ->" className="proveTheorem" />
    </div>
  </div>;
};