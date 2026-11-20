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

  it("MOVE-CORRESPONDING for internal tables, one row per source row", async () => {
    const code = `
TYPES: BEGIN OF ty_src,
         a TYPE c LENGTH 3,
         b TYPE c LENGTH 3,
       END OF ty_src,
       BEGIN OF ty_dst,
         b TYPE c LENGTH 3,
         d TYPE c LENGTH 3,
       END OF ty_dst.
DATA src TYPE STANDARD TABLE OF ty_src WITH EMPTY KEY.
DATA dst TYPE STANDARD TABLE OF ty_dst WITH EMPTY KEY.
DATA row_src TYPE ty_src.
DATA row_dst TYPE ty_dst.
row_src-a = 'a1'. row_src-b = 'b1'. APPEND row_src TO src.
row_src-a = 'a2'. row_src-b = 'b2'. APPEND row_src TO src.
row_dst-b = 'old'. row_dst-d = 'd0'. APPEND row_dst TO dst.
MOVE-CORRESPONDING src TO dst.
LOOP AT dst INTO row_dst.
  WRITE / |{ row_dst-b }/{ row_dst-d }|.
ENDLOOP.
row_dst-b = 'new'. row_dst-d = 'd9'. APPEND row_dst TO dst.
MOVE-CORRESPONDING src TO dst KEEPING TARGET LINES.
WRITE / lines( dst ).`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("b1/\nb2/\n5");
  });

});