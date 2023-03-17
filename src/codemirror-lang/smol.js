import {parser} from "./parser.js"
import {foldNodeProp, foldInside, indentNodeProp} from "@codemirror/language"
import {styleTags, tags as t} from "@lezer/highlight"

let parserWithMetadata = parser.configure({
  props: [
    styleTags({
      Identifier: t.variableName,
      Boolean: t.bool,
      String: t.string,
      LineComment: t.lineComment,
      "( )": t.paren,
      "[ ]": t.paren,
    }),
    indentNodeProp.add({
      Application: context => context.column(context.node.from) + context.unit
    }),
    foldNodeProp.add({
      Application: foldInside
    })
  ]
})

import {LRLanguage} from "@codemirror/language"

export const smolLanguage = LRLanguage.define({
  parser: parserWithMetadata,
  languageData: {
    commentTokens: {line: ";"}
  }
})

import {completeFromList} from "@codemirror/autocomplete"

export const smolCompletion = smolLanguage.data.of({
  autocomplete: completeFromList([
    {label: "deffun", type: "keyword"},
    {label: "defvar", type: "keyword"},
    {label: "set!", type: "keyword"},
    {label: "let", type: "keyword"},
    {label: "cond", type: "keyword"},
    {label: "begin", type: "keyword"},
    {label: "lambda", type: "keyword"},
    {label: "+", type: "function"},
    {label: "-", type: "function"},
    {label: "*", type: "function"},
    {label: "/", type: "function"},
    {label: "<", type: "function"},
    {label: "=", type: "function"},
    {label: ">", type: "function"},
    {label: "<=", type: "function"},
    {label: ">=", type: "function"},
    {label: "!=", type: "function"},
    {label: "eqv?", type: "function"},
    {label: "vec", type: "function"},
    {label: "mvec", type: "function"},
    {label: "vec-ref", type: "function"},
    {label: "vec-set!", type: "function"},
    {label: "vec-len", type: "function"},
    {label: "error", type: "function"}
  ])
})

import {LanguageSupport} from "@codemirror/language"

export function smol() {
  return new LanguageSupport(smolLanguage, [smolCompletion])
}
