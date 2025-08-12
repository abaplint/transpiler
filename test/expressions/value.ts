import {expect} from "chai";
import {ABAP, MemoryConsole} from "../../packages/runtime/src";
import {AsyncFunction, runFiles} from "../_utils";

let abap: ABAP;

async function run(contents: string, skipVersionCheck = false) {
  return runFiles(abap, [{filename: "zfoobar_value.prog.abap", contents}], {skipVersionCheck});
}

describe("Running expressions - VALUE", () => {

  beforeEach(async () => {
    abap = new ABAP({console: new MemoryConsole()});
  });

  it.only("basic", async () => {
    const code = `
TYPES: BEGIN OF ty,
         bar TYPE i,
       END OF ty.
DATA val TYPE ty.
val = VALUE #( bar = 2 ).
WRITE val-bar.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("2");
  });

});