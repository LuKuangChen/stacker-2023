import { EditorView } from "@codemirror/view";

export const noActiveLine = EditorView.theme({
    ".cm-activeLineGutter, .cm-activeLine": {
        backgroundColor: "unset"
    }
});
