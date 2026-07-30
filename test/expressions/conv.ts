import {expect} from "chai";
import {ABAP, MemoryConsole} from "../../packages/runtime/src";
import {AsyncFunction, runFiles} from "../_utils";

let abap: ABAP;

async function run(contents: string, skipVersionCheck = false) {
  return runFiles(abap, [{filename: "zfoobar_conv.prog.abap", contents}], {skipVersionCheck});
}

describe("Running expressions - CONV", () => {

  beforeEach(async () => {
    abap = new ABAP({console: new MemoryConsole()});
  });

  it("basic", async () => {
    const code = `
DATA int TYPE i.
int = CONV i( '123' ).
WRITE / int.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("123");
  });

  it("inferred", async () => {
    const code = `
DATA int TYPE i.
int = CONV #( '123' ).
WRITE / int.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("123");
  });

  it("type reference", async () => {
    const code = `
TYPES ty TYPE i.
DATA int TYPE i.
int = CONV ty( '123' ).
WRITE / int.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("123");
  });

  it("inline declaration in FORM", async () => {
    const code = `
FORM run.
  DATA(sdf) = CONV decfloat34( 1 ).
  WRITE / sdf.
ENDFORM.

START-OF-SELECTION.
  PERFORM run.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("1");
  });

// todo: LET
// todo: test concat and arithmetics

});
