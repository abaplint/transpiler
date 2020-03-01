import * as monaco from "monaco-editor";
import "./index.css";
import {Transpiler} from "@abaplint/transpiler";
import * as abap from "@abaplint/runtime";

// @ts-ignore
self.MonacoEnvironment = {
  getWorkerUrl: function(_moduleId, label) {
    if (label === "typescript" || label === "javascript") {
      return "./ts.worker.bundle.js";
    }
    return "./editor.worker.bundle.js";
  },
};

const editor1 = monaco.editor.create(document.getElementById("container1"), {
  value: "WRITE 'hello'.",
  theme: "vs-dark",
  minimap: {
    enabled: false,
  },
  language: "abap",
});

const editor2 = monaco.editor.create(document.getElementById("container2"), {
  value: "console.log('hello world');",
  theme: "vs-dark",
  minimap: {
    enabled: false,
  },
  language: "javascript",
});

const editor3 = monaco.editor.create(document.getElementById("container3"), {
  value: "output",
  theme: "vs-dark",
  minimap: {
    enabled: false,
  },
  readOnly: true,
  language: "text",
});

function runJS() {
  const js = editor2.getValue();
  try {
    const f = new Function('abap', js);
    f(abap);
    editor3.setValue("");
  } catch (error) {
    editor3.setValue(error.message);
  }
}

function abapChanged() {
  try {
    const js = new Transpiler().run(editor1.getValue());
    editor2.setValue(js);
    runJS();
  } catch (error) {
    editor2.setValue("");
    editor3.setValue(error.message);
  }
}

editor1.onDidChangeModelContent(abapChanged);
abapChanged();