import React from 'react';
import CodeMirror from '@uiw/react-codemirror';
import { smol } from './codemirror-lang/smol';

function SMoLCodeMirror({value, onChange}) {
  return React.createElement(CodeMirror, {
      height: "800px",
      width: "400px",
      extensions: [smol()],
      value, onChange
  }, '');
}
export default SMoLCodeMirror;
