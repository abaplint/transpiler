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

  it.only("basic", async () => {
    const code = `
DATA int TYPE i.
int = CONV i( '123' ).
WRITE / int.`;
    const js = await run(code, true);
    console.dir(js);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("123");
  });

  it("inferred", async () => {
    const code = `
DATA int TYPE i.
int = CONV #( '123' ).
WRITE / int.`;
    const js = await run(code, true);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("123");
  });

// todo: LET

});