import React from 'react';
import CodeMirror from '@uiw/react-codemirror';
import { javascript } from './codemirror-lang_javascript/javascript';
import { noActiveLine } from './codemirror-no-active-line';

function JavaScriptCodeMirror({ readOnly, value, onChange }) {
  return React.createElement(CodeMirror, {
    width: "500px",
    extensions: [javascript(), ...readOnly ? [noActiveLine] : []],
    value, onChange,
    readOnly
  }, '');
}
export default JavaScriptCodeMirror;
