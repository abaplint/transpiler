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

  it("components are converted", async () => {
    const code = `
TYPES: BEGIN OF ty1,
         a TYPE c LENGTH 3,
         b TYPE string,
       END OF ty1.
TYPES: BEGIN OF ty2,
         a TYPE i,
         b TYPE c LENGTH 2,
       END OF ty2.
DATA s1 TYPE ty1.
DATA s2 TYPE ty2.
s1-a = '12'.
s1-b = |hello|.
MOVE-CORRESPONDING s1 TO s2.
WRITE / s2-a.
WRITE / s2-b.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("12\nhe");
  });

  it("target components without a source component keep their value", async () => {
    const code = `
TYPES: BEGIN OF ty1,
         a TYPE i,
         x TYPE i,
       END OF ty1.
TYPES: BEGIN OF ty2,
         b TYPE i,
         a TYPE i,
       END OF ty2.
DATA s1 TYPE ty1.
DATA s2 TYPE ty2.
s1-a = 1.
s1-x = 9.
s2-a = 7.
s2-b = 5.
MOVE-CORRESPONDING s1 TO s2.
WRITE / s2-a.
WRITE / s2-b.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("1\n5");
  });

  it("sub structure, target components without a source component keep their value", async () => {
    const code = `
TYPES: BEGIN OF ty1,
         BEGIN OF sub,
           field TYPE i,
         END OF sub,
       END OF ty1.
TYPES: BEGIN OF ty2,
         BEGIN OF sub,
           field TYPE i,
           more  TYPE i,
         END OF sub,
       END OF ty2.
DATA s1 TYPE ty1.
DATA s2 TYPE ty2.
s1-sub-field = 2.
s2-sub-more = 3.
MOVE-CORRESPONDING s1 TO s2.
WRITE / s2-sub-field.
WRITE / s2-sub-more.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("2\n3");
  });

  it("table component is assigned as a whole", async () => {
    const code = `
TYPES ty_tab TYPE STANDARD TABLE OF i WITH EMPTY KEY.
TYPES: BEGIN OF ty1,
         tab TYPE ty_tab,
         a   TYPE i,
       END OF ty1.
TYPES: BEGIN OF ty2,
         tab TYPE ty_tab,
       END OF ty2.
DATA s1 TYPE ty1.
DATA s2 TYPE ty2.
s1-tab = VALUE #( ( 1 ) ( 2 ) ).
s2-tab = VALUE #( ( 5 ) ).
MOVE-CORRESPONDING s1 TO s2.
WRITE / lines( s2-tab ).
WRITE / s2-tab[ 1 ].`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("2\n1");
  });

  it("dereferenced data reference as source", async () => {
    const code = `
TYPES: BEGIN OF ty,
         a TYPE i,
       END OF ty.
DATA s1 TYPE ty.
DATA s2 TYPE ty.
DATA ref TYPE REF TO ty.
s1-a = 4.
ref = REF #( s1 ).
MOVE-CORRESPONDING ref->* TO s2.
WRITE / s2-a.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("4");
  });

  it("INCLUDE TYPE components", async () => {
    const code = `
TYPES: BEGIN OF ty_inc,
         a TYPE i,
       END OF ty_inc.
TYPES BEGIN OF ty1.
INCLUDE TYPE ty_inc.
TYPES b TYPE i.
TYPES END OF ty1.
TYPES: BEGIN OF ty2,
         a TYPE i,
         b TYPE i,
       END OF ty2.
DATA s1 TYPE ty1.
DATA s2 TYPE ty2.
s1-a = 1.
s1-b = 2.
MOVE-CORRESPONDING s1 TO s2.
WRITE / s2-a.
WRITE / s2-b.
CLEAR s1.
MOVE-CORRESPONDING s2 TO s1.
WRITE / s1-a.
WRITE / s1-b.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("1\n2\n1\n2");
  });

  it("INCLUDE TYPE with RENAMING WITH SUFFIX", async () => {
    const code = `
TYPES: BEGIN OF ty_inc,
         a TYPE i,
       END OF ty_inc.
TYPES BEGIN OF ty1.
INCLUDE TYPE ty_inc AS grp RENAMING WITH SUFFIX _x.
TYPES END OF ty1.
TYPES: BEGIN OF ty2,
         a_x TYPE i,
         a   TYPE i,
       END OF ty2.
DATA s1 TYPE ty1.
DATA s2 TYPE ty2.
s1-a_x = 3.
MOVE-CORRESPONDING s1 TO s2.
WRITE / s2-a_x.
WRITE / s2-a.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("3\n0");
  });

  it("reference component", async () => {
    const code = `
TYPES: BEGIN OF ty,
         r TYPE REF TO i,
       END OF ty.
DATA s1 TYPE ty.
DATA s2 TYPE ty.
DATA int TYPE i VALUE 8.
s1-r = REF #( int ).
MOVE-CORRESPONDING s1 TO s2.
WRITE / s2-r->*.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("8");
  });

  it("sub structure into an elementary component of the same name", async () => {
    const code = `
TYPES: BEGIN OF ty1,
         BEGIN OF sub,
           a TYPE c LENGTH 2,
         END OF sub,
       END OF ty1.
TYPES: BEGIN OF ty2,
         sub TYPE c LENGTH 2,
       END OF ty2.
DATA s1 TYPE ty1.
DATA s2 TYPE ty2.
s1-sub-a = 'ab'.
MOVE-CORRESPONDING s1 TO s2.
WRITE / s2-sub.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("ab");
  });

  it("same source and target", async () => {
    const code = `
TYPES: BEGIN OF ty,
         a TYPE i,
       END OF ty.
DATA s TYPE ty.
s-a = 4.
MOVE-CORRESPONDING s TO s.
WRITE / s-a.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("4");
  });

  it("same total length, other component lengths, moved by name", async () => {
    const code = `
TYPES: BEGIN OF ty1,
         a TYPE c LENGTH 5,
         b TYPE c LENGTH 10,
       END OF ty1.
TYPES: BEGIN OF ty2,
         a TYPE c LENGTH 7,
         b TYPE c LENGTH 8,
       END OF ty2.
DATA s1 TYPE ty1.
DATA s2 TYPE ty2.
s1-a = 'abcde'.
s1-b = '0123456789'.
MOVE-CORRESPONDING s1 TO s2.
WRITE / |[{ s2-a }][{ s2-b }]|.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("[abcde][01234567]");
  });

  it("sub structures of the same total length, other component lengths", async () => {
    const code = `
TYPES: BEGIN OF ty_sub1,
         a TYPE c LENGTH 5,
         b TYPE c LENGTH 10,
       END OF ty_sub1.
TYPES: BEGIN OF ty_sub2,
         a TYPE c LENGTH 7,
         b TYPE c LENGTH 8,
       END OF ty_sub2.
TYPES: BEGIN OF ty1,
         sub TYPE ty_sub1,
       END OF ty1.
TYPES: BEGIN OF ty2,
         sub TYPE ty_sub2,
       END OF ty2.
DATA s1 TYPE ty1.
DATA s2 TYPE ty2.
s1-sub-a = 'abcde'.
s1-sub-b = '0123456789'.
MOVE-CORRESPONDING s1 TO s2.
WRITE / |[{ s2-sub-a }][{ s2-sub-b }]|.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("[abcde][01234567]");
  });

  it("sub structures of the same total length without common names", async () => {
    const code = `
TYPES: BEGIN OF ty_sub1,
         a TYPE c LENGTH 5,
         b TYPE c LENGTH 10,
       END OF ty_sub1.
TYPES: BEGIN OF ty_sub2,
         x TYPE c LENGTH 7,
         y TYPE c LENGTH 8,
       END OF ty_sub2.
TYPES: BEGIN OF ty1,
         sub TYPE ty_sub1,
       END OF ty1.
TYPES: BEGIN OF ty2,
         sub TYPE ty_sub2,
       END OF ty2.
DATA s1 TYPE ty1.
DATA s2 TYPE ty2.
s1-sub-a = 'abcde'.
s1-sub-b = '0123456789'.
s2-sub-x = 'old'.
MOVE-CORRESPONDING s1 TO s2.
WRITE / |[{ s2-sub-x }][{ s2-sub-y }]|.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("[old][]");
  });

  it("sub structures with the components in another order", async () => {
    const code = `
TYPES: BEGIN OF ty_sub1,
         a TYPE c LENGTH 5,
         b TYPE c LENGTH 10,
       END OF ty_sub1.
TYPES: BEGIN OF ty_sub2,
         b TYPE c LENGTH 8,
         a TYPE c LENGTH 7,
       END OF ty_sub2.
TYPES: BEGIN OF ty1,
         sub TYPE ty_sub1,
       END OF ty1.
TYPES: BEGIN OF ty2,
         sub TYPE ty_sub2,
       END OF ty2.
DATA s1 TYPE ty1.
DATA s2 TYPE ty2.
s1-sub-a = 'abcde'.
s1-sub-b = '0123456789'.
MOVE-CORRESPONDING s1 TO s2.
WRITE / |[{ s2-sub-a }][{ s2-sub-b }]|.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("[abcde][01234567]");
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