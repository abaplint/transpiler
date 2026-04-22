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
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("2");
  });

  it("basic BASE", async () => {
    const code = `
TYPES: BEGIN OF ty,
         foo TYPE i,
         bar TYPE i,
       END OF ty.
DATA data1 TYPE ty.
DATA data2 TYPE ty.
DATA data3 TYPE ty.
data2-foo = 1.
data3-foo = 2.
data2-bar = 3.
data3-bar = 4.
data1 = CORRESPONDING #( BASE ( data3 ) data2 ).
WRITE / data1-foo.
WRITE / data1-bar.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("1\n3");
  });

  it("basic MAPPING", async () => {
    const code = `
TYPES: BEGIN OF ty1,
         match TYPE i,
         bar   TYPE i,
       END OF ty1.

TYPES: BEGIN OF ty2,
         match TYPE i,
         foo   TYPE i,
       END OF ty2.

DATA data1 TYPE ty1.
DATA data2 TYPE ty2.

data2 = VALUE #( match = 1 foo = 2 ).
data1 = CORRESPONDING #( data2 MAPPING bar = foo ).
WRITE / data1-match.
WRITE / data1-bar.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("1\n2");
  });

  it("more CORRESPONDING", async () => {
    const code = `
TYPES: BEGIN OF ty_configstatus_st,
         status TYPE string,
         text   TYPE string,
       END OF ty_configstatus_st,
       ty_configstatus_tt TYPE SORTED TABLE OF ty_configstatus_st WITH UNIQUE KEY status.

FORM run.

  TYPES: BEGIN OF ty_row,
           low    TYPE string,
           ddtext TYPE string,
         END OF ty_row.

  DATA lt_tab TYPE STANDARD TABLE OF ty_row WITH DEFAULT KEY.

  INSERT VALUE #(
    low    = 'hello'
    ddtext = 'world' ) INTO TABLE lt_tab.

  DATA(lt_confstat) = CORRESPONDING ty_configstatus_tt( lt_tab MAPPING status = low text = ddtext ).

  ASSERT lines( lt_confstat ) = 1.
  WRITE / lt_confstat[ 1 ]-status.
  WRITE / lt_confstat[ 1 ]-text.

ENDFORM.

START-OF-SELECTION.
  PERFORM run.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("hello\nworld");
  });

});