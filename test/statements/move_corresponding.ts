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

  it("internal tables without header lines, line by line", async () => {
    const code = `
TYPES: BEGIN OF ty_src,
         field TYPE i,
         other TYPE c LENGTH 1,
       END OF ty_src.
TYPES: BEGIN OF ty_tgt,
         field TYPE i,
         extra TYPE i,
       END OF ty_tgt.
DATA src TYPE STANDARD TABLE OF ty_src WITH DEFAULT KEY.
DATA tgt TYPE STANDARD TABLE OF ty_tgt WITH DEFAULT KEY.
DATA row TYPE ty_src.
DATA line TYPE ty_tgt.
line-field = 99.
APPEND line TO tgt.
row-field = 1.
APPEND row TO src.
row-field = 2.
APPEND row TO src.
MOVE-CORRESPONDING src TO tgt.
WRITE lines( tgt ).
LOOP AT tgt INTO line.
  WRITE / line-field.
ENDLOOP.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("2\n1\n2");
  });

  it("tables with header lines, the header lines", async () => {
    const code = `
TYPES: BEGIN OF ty,
         field TYPE i,
       END OF ty.
DATA src TYPE STANDARD TABLE OF ty WITH HEADER LINE.
DATA tgt TYPE STANDARD TABLE OF ty WITH HEADER LINE.
src-field = 3.
APPEND src.
MOVE-CORRESPONDING src TO tgt.
WRITE tgt-field.
WRITE / lines( tgt ).`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("3\n0");
  });

});