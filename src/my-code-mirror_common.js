import React from 'react';
import CodeMirror from '@uiw/react-codemirror';
import { common } from './codemirror-lang_common/common';
import { noActiveLine } from './codemirror-no-active-line';

function commonCodeMirror({ readOnly, value, onChange }) {
  return React.createElement(CodeMirror, {
    width: "100%",
    extensions: [common(), ...readOnly ? [noActiveLine] : []],
    value, onChange,
    readOnly
  }, '');
}
export default commonCodeMirror;
