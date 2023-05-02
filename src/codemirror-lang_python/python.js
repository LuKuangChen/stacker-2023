import { parser } from "./parser.js";
import { foldNodeProp, foldInside, indentNodeProp } from "@codemirror/language";
import { styleTags, tags as t } from "@lezer/highlight";

let parserWithMetadata = parser.configure({
  props: [
    styleTags({
      Identifier: t.variableName,
      Boolean: t.bool,
      String: t.string,
      LineComment: t.lineComment,
      "( )": t.paren,
      "[ ]": t.paren,

      "True": t.keyword,
      "False": t.keyword,
      "def": t.keyword,
      "if": t.keyword,
      "elif": t.keyword,
      "else": t.keyword,
      "return": t.keyword,
      "raise": t.keyword,
    }),
    indentNodeProp.add({
      Application: context => context.column(context.node.from) + context.unit
    }),
    foldNodeProp.add({
      Application: foldInside
    })
  ]
});

import { LRLanguage } from "@codemirror/language";

export const pythonLanguage = LRLanguage.define({
  parser: parserWithMetadata,
  languageData: {
    commentTokens: { line: "//" }
  }
});

import { completeFromList } from "@codemirror/autocomplete";

export const pythonCompletion = pythonLanguage.data.of({
  autocomplete: completeFromList([

    { label: "True", type: "keyword" },
    { label: "False", type: "keyword" },
    { label: "def", type: "keyword" },
    { label: "if", type: "keyword" },
    { label: "elif", type: "keyword" },
    { label: "else", type: "keyword" },
    { label: "return", type: "keyword" },
    { label: "raise", type: "keyword" },
    { label: "lambda", type: "keyword" },
    { label: "+", type: "function" },
    { label: "-", type: "function" },
    { label: "*", type: "function" },
    { label: "/", type: "function" },
    { label: "<", type: "function" },
    { label: "is", type: "function" },
    { label: ">", type: "function" },
    { label: "<=", type: "function" },
    { label: ">=", type: "function" },
    { label: "!=", type: "function" }
  ])
});

import { LanguageSupport } from "@codemirror/language";

export function python() {
  return new LanguageSupport(pythonLanguage, [pythonCompletion]);
}
