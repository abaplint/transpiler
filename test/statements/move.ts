import {expect} from "chai";
import {ABAP, MemoryConsole} from "../../packages/runtime/src";
import {AsyncFunction, runFiles} from "../_utils";

let abap: ABAP;

async function run(contents: string, skipVersionCheck = false) {
  return runFiles(abap, [{filename: "zfoobar_move.prog.abap", contents}], {skipVersionCheck});
}

describe("Running statements - MOVE", () => {

  beforeEach(async () => {
    abap = new ABAP({console: new MemoryConsole()});
  });

  it("MOVE - integer to integer", async () => {
    const code = `
        data lv_int1 type i value 1.
        data lv_int2 type i value 2.
        move lv_int1 to lv_int2.
        write lv_int2.
    `;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("1");
  });

  it("MOVE - integer to string", async () => {
    const code = `
        data lv_int1 type i value 1.
        data lv_string type string value ''.
        move lv_int1 to lv_string.
        write lv_string.
    `;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("1 ");
  });

  it("Chained assignment", async () => {
    const code = `
    DATA foo TYPE i.
    DATA bar TYPE i.
    foo = bar = 2.
    WRITE foo.
    WRITE bar.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("22");
  });

  it("Chained assignment, truncating", async () => {
    const code = `
DATA foo1 TYPE c LENGTH 10.
DATA foo2 TYPE c LENGTH 5.
DATA foo3 TYPE c LENGTH 10.

foo3 = 'HELLOWORLD'.

foo1 = foo2 = foo3.
WRITE / foo1.
WRITE / foo2.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("HELLO     \nHELLO");
  });

  it("namespaced field", async () => {
    const code = `
DATA: BEGIN OF foo,
        /bar/moo TYPE i,
      END OF foo.
foo-/bar/moo = 2.
WRITE foo-/bar/moo.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("2");
  });

  it("move structured into basic", async () => {
    const code = `
TYPES: BEGIN OF ty,
         field1 TYPE c LENGTH 2,
         field2 TYPE c LENGTH 2,
       END OF ty.
DATA foo TYPE ty.
DATA char TYPE c LENGTH 1.
foo-field1 = 'HE'.
char = foo.
WRITE char.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("H");
  });

  it("move structured field symbol into basic", async () => {
    const code = `
TYPES: BEGIN OF ty,
         field1 TYPE c LENGTH 2,
         field2 TYPE c LENGTH 2,
       END OF ty.
DATA foo TYPE ty.
DATA char TYPE c LENGTH 1.
FIELD-SYMBOLS <fs> TYPE ty.
ASSIGN foo TO <fs>.
foo-field1 = 'HE'.
char = <fs>.
WRITE char.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("H");
  });

  it.skip("MOVE - integer to integer structure", async () => {
    const code = `
        TYPES:
        BEGIN OF ts_str,
        comp_one TYPE i,
        END OF ts_str.
    DATA lv_int1 TYPE i VALUE 1.
    DATA lv_str TYPE ts_str.

    FIELD-SYMBOLS <ls_str> type any.
    ASSIGN lv_str to <ls_str>.
    MOVE lv_int1 TO <ls_str>.
    WRITE <ls_str>.
    `;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);

    expect(await f(abap)).to.throw();
  });

  it.skip("MOVE - string to integer structure", async () => {
    const code = `
    TYPES: BEGIN OF ts_str,
             comp_one TYPE i,
           END OF ts_str.
    DATA lv_int1 TYPE i VALUE 1.
    DATA lv_str TYPE ts_str.

    FIELD-SYMBOLS <ls_str> type any.
    ASSIGN lv_str to <ls_str>.
    MOVE lv_int1 TO <ls_str>.
    WRITE <ls_str>.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);

    expect(await f(abap)).to.throw();
  });

  it("move +=", async () => {
    const code = `
data foo type i.
foo = 1.
foo += 2.
write foo.`;
    const js = await run(code, true);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("3");
  });

  it("move -=", async () => {
    const code = `
data foo type i.
foo = 5.
foo -= 1.
write foo.`;
    const js = await run(code, true);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("4");
  });

  it("move /=", async () => {
    const code = `
data foo type i.
foo = 4.
foo /= 2.
write foo.`;
    const js = await run(code, true);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("2");
  });

  it("move *=", async () => {
    const code = `
data foo type i.
foo = 4.
foo *= 2.
write foo.`;
    const js = await run(code, true);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("8");
  });

  it("move &&=", async () => {
    const code = `
data foo type string.
foo = 'hello'.
foo &&= 'world'.
write foo.`;
    const js = await run(code, true);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("helloworld");
  });

});