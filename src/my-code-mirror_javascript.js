import React from 'react';
import CodeMirror from '@uiw/react-codemirror';
import { javascript } from './codemirror-lang_javascript/javascript';

function JavaScriptCodeMirror({ readOnly, value, onChange }) {
  return React.createElement(CodeMirror, {
    width: "500px",
    extensions: [javascript()],
    value, onChange,
    readOnly
  }, '');
}
export default JavaScriptCodeMirror;
