import "./index.css";
import * as monaco from "monaco-editor";
import {config, Transpiler} from "@abaplint/transpiler";
import * as runtime from "@abaplint/runtime";
import * as abaplint from "@abaplint/core";
import * as abapMonaco from "@abaplint/monaco";
import Split from "split-grid";

// @ts-ignore
self.MonacoEnvironment = {
  getWorkerUrl: function(_moduleId, label) {
    if (label === "typescript" || label === "javascript") {
      return "./ts.worker.bundle.js";
    }
    return "./editor.worker.bundle.js";
  },
};

const reg = new abaplint.Registry(new abaplint.Config(JSON.stringify(config)));
abapMonaco.registerABAP(reg);

const filename = "file:///zfoobar.prog.abap";
const model1 = monaco.editor.createModel(
  "WRITE 'hello'.",
  "abap",
  monaco.Uri.parse(filename),
);
reg.addFile(new abaplint.MemoryFile(filename, ""));

Split({
  columnGutters: [
    {
      track: 1,
      element: document.getElementById("gutter1"),
    },
    {
      track: 3,
      element: document.getElementById("gutter2"),
    },
  ],
});

const editor1 = monaco.editor.create(document.getElementById("container1"), {
  model: model1,
  theme: "vs-dark",
  minimap: {
    enabled: false,
  },
});

const editor2 = monaco.editor.create(document.getElementById("container2"), {
  value: "js",
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

function updateEditorLayouts() {
  editor1.layout();
  editor2.layout();
  editor3.layout();
}

const observer = new MutationObserver(mutations => {
  for (const mutation of mutations) {
    if (mutation.attributeName === "style") {
      updateEditorLayouts();
    }
  }
});

observer.observe(document.getElementById("horizon"), {
  attributes: true,
  attributeFilter: [
    "style",
  ],
});

window.addEventListener("resize", updateEditorLayouts);

function jsChanged() {
  const js = editor2.getValue();
  try {
    runtime.Console.clear();
    try {
      const f = new Function("abap", js);
      f(runtime);
      editor3.setValue(runtime.Console.get());
    } catch(e) {
      // write all errors to runtime result
      editor3.setValue("An error was thrown: " + e.toString());
    }
  } catch (error) {
    editor3.setValue(error.message);
  }
}

async function abapChanged() {
  try {
    const contents = editor1.getValue();
    const file = new abaplint.MemoryFile(filename, contents);
    reg.updateFile(file);
    reg.parse();
    abapMonaco.updateMarkers(reg, model1);

    const res = await new Transpiler().run([{filename, contents}]);
    editor2.setValue(res[0]?.js.contents);
  } catch (error) {
    editor2.setValue("");
    editor3.setValue(error.message);
  }
}

editor1.onDidChangeModelContent(abapChanged);
editor2.onDidChangeModelContent(jsChanged);
abapChanged();
editor1.focus();