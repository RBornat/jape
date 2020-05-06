[@react.component]
let app = () =>
    <div className="app">
        <div className="theoremBrowser">
            <h4>{React.string("My theorems")}</h4>
            <div className="file">
                <p className="fileName">{React.string("Some_file.j")}</p>
            </div>
        </div>
        <div className="theoremEditor">
        <textarea/>
        </div>
    </div>