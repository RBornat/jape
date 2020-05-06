[@react.component]
let theoremEditor = () => {
    let (currentTheorem, setCurrentTheorem) = React.useState(() => []);
    <div className="theoremEditor">
        {React.array(Array.of_list(
            List.map(_=><input/>, currentTheorem)
        ))}
        <button className="newLineButton" onClick={_ => setCurrentTheorem((old) => ["", ...old])}/>
    </div>
}
    