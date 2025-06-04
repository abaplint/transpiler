// @ts-ignore
global.MonacoEnvironment = {
  globalAPI: true,
  getWorkerUrl: function (_moduleId: any, label: any) {
    if (label === "json") {
      return "./json.worker.bundle.js";
    }
    if (label === "typescript" || label === "javascript") {
      return "./ts.worker.bundle.js";
    }
    return "./editor.worker.bundle.js";
  },
};

import "./index.css";
import "../public/favicon-16x16.png";
import "../public/favicon-32x32.png";
import * as monaco from "monaco-editor";
import {config, Transpiler} from "@abaplint/transpiler";
import {ABAP, MemoryConsole} from "@abaplint/runtime";
import * as abaplint from "@abaplint/core";
import * as abapMonaco from "@abaplint/monaco";
import Split from "split-grid";

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
editor1.addCommand(
	monaco.KeyCode.F9,
  () => {
		console.log("my command is executing!");
	},
);

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

// see https://github.com/SimulatedGREG/electron-vue/issues/777
// see https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/AsyncFunction
const AsyncFunction = new Function(`return Object.getPrototypeOf(async function(){}).constructor`)();

async function jsChanged() {
  const makeGlobal = "abap = abapLocal;\n";
  const js = makeGlobal + editor2.getValue();
  try {
    abap.console.clear();
    try {
      const f = new AsyncFunction("abapLocal", js);
      await f(abap);
      editor3.setValue(abap.console.get());
    } catch(e) {
      // write all errors to runtime result
      editor3.setValue("An error was thrown: " + e.toString());
    }
  } catch (error) {
    editor3.setValue(error.message);
    console.dir(error);
  }
}

async function abapChanged() {
  try {
    const contents = editor1.getValue();
    const file = new abaplint.MemoryFile(filename, contents);
    reg.updateFile(file);
    reg.parse();
    abapMonaco.updateMarkers(reg, model1);

    const res = await new Transpiler().runRaw([{filename, contents}]);
    const obj = res.objects[0];
    const chunk = obj.chunk;
    console.dir(chunk.getMap(obj.filename));
    editor2.setValue(obj.chunk.getCode() || "");
  } catch (error) {
    editor2.setValue("");
    editor3.setValue(error.message);
    console.dir(error);
  }
}

editor1.onDidChangeModelContent(abapChanged);
editor2.onDidChangeModelContent(jsChanged);
abapChanged();
editor1.focus();
const abap = new ABAP({console: new MemoryConsole()});
