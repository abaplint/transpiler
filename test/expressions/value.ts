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

  it("basic, single field", async () => {
    const code = `
TYPES: BEGIN OF ty,
         bar TYPE i,
       END OF ty.
DATA val TYPE ty.
val = VALUE #( bar = 2 ).
WRITE val-bar.`;
    const js = await run(code, true);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("2");
  });

  it("basic, two fields", async () => {
    const code = `
TYPES: BEGIN OF ty,
         bar TYPE i,
         baz TYPE i,
       END OF ty.
DATA val TYPE ty.
val = VALUE #( bar = 2 baz = 3 ).
WRITE val-bar.
WRITE val-baz.`;
    const js = await run(code, true);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("23");
  });

});