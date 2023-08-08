import {expect} from "chai";
import {ABAP, MemoryConsole} from "../../packages/runtime/src";
import {AsyncFunction, runFiles} from "../_utils";

let abap: ABAP;

async function run(contents: string) {
  return runFiles(abap, [{filename: "zfoobar.prog.abap", contents}]);
}

describe("Running statements - CONSTANTS", () => {

  beforeEach(async () => {
    abap = new ABAP({console: new MemoryConsole()});
  });

  it("simple", async () => {
    const code = `
    CONSTANTS cval TYPE string VALUE 'ab'.
    WRITE cval.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("ab");
  });

  it("concat", async () => {
    const code = `
    CONSTANTS cval TYPE string VALUE 'a' & 'b'.
    WRITE cval.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("ab");
  });

  it("newline constant", async () => {
    const code = `
constants new type c length 1 value %_NEWLINE.
write / new.
write / %_NEWLINE.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
  });

  it("newline constant double", async () => {
    const code = `
CONSTANTS bar TYPE c LENGTH 1 VALUE %_newline.
CONSTANTS foo TYPE c LENGTH 1 VALUE bar.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
  });

  it("newline classes", async () => {
    const code = `
CLASS lcl_char DEFINITION.
  PUBLIC SECTION.
    CONSTANTS blah TYPE c LENGTH 1 VALUE %_newline.
ENDCLASS.

CLASS lcl_char IMPLEMENTATION.
ENDCLASS.

CLASS lcl_more DEFINITION.
  PUBLIC SECTION.
    CONSTANTS blah TYPE c LENGTH 1 VALUE lcl_char=>blah.
ENDCLASS.

CLASS lcl_more IMPLEMENTATION.
ENDCLASS.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
  });

});