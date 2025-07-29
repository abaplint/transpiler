import {expect} from "chai";
import {ABAP, MemoryConsole} from "../../packages/runtime/src";
import {AsyncFunction, runFiles} from "../_utils";

let abap: ABAP;

async function run(contents: string) {
  return runFiles(abap, [{filename: "zfoobar.prog.abap", contents}]);
}

describe("Running statements - MOVE-CORRESPONDING", () => {

  beforeEach(async () => {
    abap = new ABAP({console: new MemoryConsole()});
  });

  it("Basic", async () => {
    const code = `
TYPES: BEGIN OF ty,
         field TYPE i,
       END OF ty.
DATA bar TYPE ty.
DATA foo TYPE ty.
bar-field = 2.
MOVE-CORRESPONDING bar TO foo.
WRITE foo-field.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("2");
  });

  it("sub structures with more fields", async () => {
    const code = `
TYPES: BEGIN OF ty1,
         BEGIN OF sub,
           field TYPE i,
           more  TYPE i,
         END OF sub,
       END OF ty1.

TYPES: BEGIN OF ty2,
         BEGIN OF sub,
           field TYPE i,
         END OF sub,
       END OF ty2.

DATA data1 TYPE ty1.
DATA data2 TYPE ty2.

data1-sub-field = 2.
MOVE-CORRESPONDING data1 TO data2.
WRITE / data2-sub-field.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("2");
  });

  it("FS target", async () => {
    const code = `
TYPES: BEGIN OF ty,
         field TYPE i,
       END OF ty.
DATA val1 TYPE ty.
DATA val2 TYPE ty.
FIELD-SYMBOLS <fs> TYPE any.
ASSIGN val2 TO <fs>.
MOVE-CORRESPONDING val1 TO <fs>.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
  });

  it("FS source", async () => {
    const code = `
TYPES: BEGIN OF ty,
         field TYPE i,
       END OF ty.
DATA val1 TYPE ty.
DATA val2 TYPE ty.
FIELD-SYMBOLS <fs> TYPE any.
ASSIGN val2 TO <fs>.
MOVE-CORRESPONDING <fs> TO val1.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
  });

});