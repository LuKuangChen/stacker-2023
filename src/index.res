/*

Let this module does all the bootstrap.

*/

%%raw("import './styles/index.css'")
%%raw("import './blocklyEditor.js'")

switch ReactDOM.querySelector("#root") {
| Some(rootElement) => {
    let root = ReactDOM.Client.createRoot(rootElement)
    ReactDOM.Client.Root.render(root, <App />)
  }
| None => ()
}