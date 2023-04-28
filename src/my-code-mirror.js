import React from 'react';
import CodeMirror from '@uiw/react-codemirror';
import { smol } from './codemirror-lang/smol';

function SMoLCodeMirror({ readOnly, value, onChange }) {
  return React.createElement(CodeMirror, {
    width: "400px",
    extensions: [smol()],
    value, onChange,
    readOnly
  }, '');
}
export default SMoLCodeMirror;
