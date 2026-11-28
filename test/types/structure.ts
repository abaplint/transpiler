import {expect} from "chai";
import {ABAP, MemoryConsole} from "../../packages/runtime/src/";
import {AsyncFunction, runFiles} from "../_utils";

let abap: ABAP;

async function run(contents: string) {
  return runFiles(abap, [{filename: "zfoobar.prog.abap", contents}]);
}

describe("Running Examples - Structure type", () => {

  beforeEach(async () => {
    abap = new ABAP({console: new MemoryConsole()});
  });

  it("Move, non matching columns", async () => {
    const code = `
TYPES: BEGIN OF alphatab_type,
         cola TYPE string,
       END OF alphatab_type.
TYPES: BEGIN OF combined_data_type,
         colx TYPE string,
       END OF combined_data_type.
DATA foo1 TYPE alphatab_type.
DATA foo2 TYPE combined_data_type.
foo1 = foo2.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
  });

  it("Move, cstructure into string", async () => {
    const code = `
TYPES: BEGIN OF ty,
         name TYPE c LENGTH 10,
       END OF ty.
DATA row TYPE ty.
DATA key_name TYPE string.
row-name = 'hello'.
key_name = row.
WRITE key_name.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get().trimEnd()).to.equal("hello");
  });

  it("Move, flat structures not compatible, character fragments", async () => {
    const code = `
TYPES: BEGIN OF t1s, a TYPE c LENGTH 10, END OF t1s,
       BEGIN OF t1t, a TYPE c LENGTH 5, b TYPE c LENGTH 5, END OF t1t.
TYPES: BEGIN OF t3s, a TYPE c LENGTH 4, END OF t3s,
       BEGIN OF t3t, a TYPE c LENGTH 2, b TYPE c LENGTH 4, END OF t3t.
TYPES: BEGIN OF t4s, a TYPE c LENGTH 3, i TYPE i, b TYPE c LENGTH 2, END OF t4s,
       BEGIN OF t4t, x TYPE c LENGTH 2, y TYPE c LENGTH 1, i TYPE i, z TYPE c LENGTH 4, END OF t4t.
TYPES: BEGIN OF t5s, a TYPE c LENGTH 4, END OF t5s,
       BEGIN OF t5t, x TYPE c LENGTH 1, y TYPE n LENGTH 3, END OF t5t.
TYPES: BEGIN OF t6sub, b TYPE c LENGTH 2, c TYPE c LENGTH 2, END OF t6sub,
       BEGIN OF t6s, a TYPE c LENGTH 2, sub TYPE t6sub, END OF t6s,
       BEGIN OF t6t, x TYPE c LENGTH 3, y TYPE c LENGTH 3, END OF t6t.
TYPES: BEGIN OF t8s, a TYPE c LENGTH 14, END OF t8s,
       BEGIN OF t8t, d TYPE d, t TYPE t, END OF t8t.
DATA s1 TYPE t1s.
DATA t1 TYPE t1t.
DATA s2 TYPE t1t.
DATA t2 TYPE t1s.
DATA s3 TYPE t3s.
DATA t3 TYPE t3t.
DATA s4 TYPE t4s.
DATA t4 TYPE t4t.
DATA s5 TYPE t5s.
DATA t5 TYPE t5t.
DATA s6 TYPE t6s.
DATA t6 TYPE t6t.
DATA s8 TYPE t8s.
DATA t8 TYPE t8t.

s1-a = 'asdasd'.
MOVE s1 TO t1.
WRITE / |{ t1-a }/{ t1-b }|.

s2-a = 'abcde'.
s2-b = 'fghij'.
t2 = s2.
WRITE / |{ t2-a }|.

s3-a = 'abcd'.
t3-a = 'XX'.
t3-b = 'YYYY'.
t3 = s3.
WRITE / |{ t3-a }/{ t3-b }|.

s4-a = 'abc'.
s4-i = 42.
s4-b = 'de'.
t4-z = 'ZZZZ'.
t4 = s4.
WRITE / |{ t4-x }/{ t4-y }/{ t4-i }/{ t4-z }|.

s5-a = 'a123'.
t5 = s5.
WRITE / |{ t5-x }/{ t5-y }|.

s6-a = 'ab'.
s6-sub-b = 'cd'.
s6-sub-c = 'ef'.
t6 = s6.
WRITE / |{ t6-x }/{ t6-y }|.

s8-a = '20261119123456'.
t8 = s8.
WRITE / |{ t8-d }/{ t8-t }|.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.getTrimmed()).to.equal(
      "asdas/d\nabcdefghij\nab/cd\nab/c/42/de\na/123\nabc/def\n20261119/123456");
  });

});