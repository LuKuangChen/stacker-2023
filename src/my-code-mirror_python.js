import React from 'react';
import CodeMirror from '@uiw/react-codemirror';
import { python } from './codemirror-lang_python/python';
import { noActiveLine } from './codemirror-no-active-line';

function PythonCodeMirror({ readOnly, value, onChange }) {
  return React.createElement(CodeMirror, {
    width: "100%",
    extensions: [python(), ...readOnly ? [noActiveLine] : []],
    value, onChange,
    readOnly
  }, '');
}
export default PythonCodeMirror;
