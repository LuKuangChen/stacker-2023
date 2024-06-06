import React from 'react';
import CodeMirror from '@uiw/react-codemirror';
import { smol } from './codemirror-lang_smol/smol';
import { noActiveLine } from './codemirror-no-active-line';

function SMoLCodeMirror({ readOnly, value, onChange }) {
  return React.createElement(CodeMirror, {
    width: "100%",
    extensions: [smol(), ...readOnly ? [noActiveLine] : []],
    value, onChange,
    readOnly
  }, '');
}
export default SMoLCodeMirror;
