import {expect} from "chai";
import {ABAP, MemoryConsole} from "../../packages/runtime/src";
import {AsyncFunction, runFiles} from "../_utils";

let abap: ABAP;

async function run(contents: string, skipVersionCheck = false) {
  return runFiles(abap, [{filename: "zfoobar_corresponding.prog.abap", contents}], {skipVersionCheck});
}

describe("Running expressions - CORRESPONDING", () => {

  beforeEach(async () => {
    abap = new ABAP({console: new MemoryConsole()});
  });

  it("basic", async () => {
    const code = `
TYPES: BEGIN OF ty,
         foo TYPE i,
         bar TYPE i,
       END OF ty.
DATA data1 TYPE ty.
DATA data2 TYPE ty.
data2-foo = 2.
data1 = CORRESPONDING #( data2 ).
WRITE / data1-foo.`;
    const js = await run(code, true);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("2");
  });

// next: BASE

});