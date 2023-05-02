import React from 'react';
import CodeMirror from '@uiw/react-codemirror';
import { python } from './codemirror-lang_python/python';

function PythonCodeMirror({ readOnly, value, onChange }) {
  return React.createElement(CodeMirror, {
    width: "500px",
    extensions: [python()],
    value, onChange,
    readOnly
  }, '');
}
export default PythonCodeMirror;
