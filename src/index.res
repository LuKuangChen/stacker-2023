/*

Let this module does all the bootstrap.

*/

%%raw("import './styles/index.css'")

switch ReactDOM.querySelector("#root") {
| Some(rootElement) => {
    let root = ReactDOM.Client.createRoot(rootElement)
    ReactDOM.Client.Root.render(root, <Root />)
  }
| None => ()
}