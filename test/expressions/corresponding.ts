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

  it("target components without a source component are initial", async () => {
    const code = `
TYPES: BEGIN OF ty1,
         foo TYPE i,
       END OF ty1.
TYPES: BEGIN OF ty2,
         foo TYPE i,
         bar TYPE i,
       END OF ty2.
DATA s1 TYPE ty1.
DATA s2 TYPE ty2.
s1-foo = 1.
s2-bar = 5.
s2 = CORRESPONDING #( s1 ).
WRITE / s2-foo.
WRITE / s2-bar.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("1\n0");
  });

  it("sub structures by name", async () => {
    const code = `
TYPES: BEGIN OF ty1,
         BEGIN OF sub,
           a TYPE i,
           b TYPE i,
         END OF sub,
       END OF ty1.
TYPES: BEGIN OF ty2,
         BEGIN OF sub,
           a TYPE i,
         END OF sub,
       END OF ty2.
DATA s1 TYPE ty1.
DATA s2 TYPE ty2.
s1-sub-a = 1.
s2 = CORRESPONDING #( s1 ).
WRITE / s2-sub-a.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("1");
  });

  it("MAPPING swaps two components", async () => {
    const code = `
TYPES: BEGIN OF ty,
         foo TYPE i,
         bar TYPE i,
       END OF ty.
DATA s1 TYPE ty.
DATA s2 TYPE ty.
s1 = VALUE #( foo = 1 bar = 2 ).
s2 = CORRESPONDING #( s1 MAPPING foo = bar bar = foo ).
WRITE / s2-foo.
WRITE / s2-bar.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("2\n1");
  });

  it("BASE and MAPPING", async () => {
    const code = `
TYPES: BEGIN OF ty1,
         a TYPE i,
       END OF ty1.
TYPES: BEGIN OF ty2,
         a TYPE i,
         b TYPE i,
         c TYPE i,
       END OF ty2.
DATA s1 TYPE ty1.
DATA s2 TYPE ty2.
s1-a = 1.
s2 = VALUE #( a = 7 b = 8 c = 9 ).
s2 = CORRESPONDING #( BASE ( s2 ) s1 MAPPING b = a ).
WRITE / s2-a.
WRITE / s2-b.
WRITE / s2-c.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("1\n1\n9");
  });

  it("field symbol source with MAPPING", async () => {
    const code = `
TYPES: BEGIN OF ty1,
         foo TYPE i,
       END OF ty1.
TYPES: BEGIN OF ty2,
         bar TYPE i,
       END OF ty2.
DATA s1 TYPE ty1.
DATA s2 TYPE ty2.
FIELD-SYMBOLS <fs> TYPE ty1.
s1-foo = 6.
ASSIGN s1 TO <fs>.
s2 = CORRESPONDING #( <fs> MAPPING bar = foo ).
WRITE / s2-bar.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("6");
  });

  it("field symbol table source", async () => {
    const code = `
TYPES: BEGIN OF ty,
         a TYPE i,
       END OF ty.
TYPES ty_tab TYPE STANDARD TABLE OF ty WITH EMPTY KEY.
DATA t1 TYPE ty_tab.
DATA t2 TYPE ty_tab.
FIELD-SYMBOLS <fs> TYPE ty_tab.
t1 = VALUE #( ( a = 1 ) ( a = 2 ) ).
ASSIGN t1 TO <fs>.
t2 = CORRESPONDING #( <fs> ).
WRITE / lines( t2 ).`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("2");
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
s2 = CORRESPONDING #( ref->* ).
WRITE / s2-a.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("4");
  });

  it("table expression as source", async () => {
    const code = `
TYPES: BEGIN OF ty1,
         a TYPE i,
         b TYPE i,
       END OF ty1.
TYPES: BEGIN OF ty2,
         a TYPE i,
       END OF ty2.
TYPES ty_tab TYPE STANDARD TABLE OF ty1 WITH EMPTY KEY.
DATA t1 TYPE ty_tab.
DATA s2 TYPE ty2.
t1 = VALUE #( ( a = 1 b = 2 ) ( a = 3 b = 4 ) ).
s2 = CORRESPONDING #( t1[ 2 ] ).
WRITE / s2-a.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("3");
  });

  it("type name with inline declaration", async () => {
    const code = `
TYPES: BEGIN OF ty1,
         a TYPE i,
         b TYPE i,
       END OF ty1.
TYPES: BEGIN OF ty2,
         a TYPE i,
       END OF ty2.
DATA s1 TYPE ty1.
s1-a = 5.
DATA(s2) = CORRESPONDING ty2( s1 ).
WRITE / s2-a.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("5");
  });

  it("table, MAPPING into the source table itself", async () => {
    const code = `
TYPES: BEGIN OF ty,
         a TYPE i,
         b TYPE i,
       END OF ty.
TYPES ty_tab TYPE STANDARD TABLE OF ty WITH EMPTY KEY.
DATA t1 TYPE ty_tab.
t1 = VALUE #( ( a = 1 b = 2 ) ).
t1 = CORRESPONDING #( t1 MAPPING b = a ).
WRITE / t1[ 1 ]-a.
WRITE / t1[ 1 ]-b.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("1\n1");
  });

  it("structure, MAPPING into the source structure itself", async () => {
    const code = `
TYPES: BEGIN OF ty,
         a TYPE i,
         b TYPE i,
       END OF ty.
DATA s TYPE ty.
s = VALUE #( a = 1 b = 2 ).
s = CORRESPONDING #( s MAPPING a = b b = a ).
WRITE / s-a.
WRITE / s-b.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("2\n1");
  });

  it("hashed target table", async () => {
    const code = `
TYPES: BEGIN OF ty1,
         a TYPE i,
         b TYPE i,
       END OF ty1.
TYPES: BEGIN OF ty2,
         a TYPE i,
       END OF ty2.
TYPES ty_tab1 TYPE STANDARD TABLE OF ty1 WITH EMPTY KEY.
TYPES ty_tab2 TYPE HASHED TABLE OF ty2 WITH UNIQUE KEY a.
DATA t1 TYPE ty_tab1.
DATA t2 TYPE ty_tab2.
t1 = VALUE #( ( a = 2 b = 1 ) ( a = 1 b = 2 ) ).
t2 = CORRESPONDING #( t1 ).
WRITE / lines( t2 ).
WRITE / t2[ a = 1 ]-a.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("2\n1");
  });

  it("table component without DEEP", async () => {
    const code = `
TYPES: BEGIN OF ty_row,
         x TYPE i,
       END OF ty_row.
TYPES ty_tab TYPE STANDARD TABLE OF ty_row WITH EMPTY KEY.
TYPES: BEGIN OF ty1,
         tab TYPE ty_tab,
         a   TYPE i,
       END OF ty1.
TYPES: BEGIN OF ty2,
         tab TYPE ty_tab,
       END OF ty2.
DATA s1 TYPE ty1.
DATA s2 TYPE ty2.
s1-tab = VALUE #( ( x = 1 ) ( x = 2 ) ).
s2 = CORRESPONDING #( s1 ).
WRITE / lines( s2-tab ).`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("2");
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
s2 = CORRESPONDING #( s1 ).
WRITE / |[{ s2-a }][{ s2-b }]|.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("[abcde][01234567]");
  });

  it("table rows of the same total length, other component lengths", async () => {
    const code = `
TYPES: BEGIN OF ty1,
         a TYPE c LENGTH 5,
         b TYPE c LENGTH 10,
       END OF ty1.
TYPES: BEGIN OF ty2,
         a TYPE c LENGTH 7,
         b TYPE c LENGTH 8,
       END OF ty2.
DATA t1 TYPE STANDARD TABLE OF ty1 WITH EMPTY KEY.
DATA t2 TYPE STANDARD TABLE OF ty2 WITH EMPTY KEY.
t1 = VALUE #( ( a = 'abcde' b = '0123456789' ) ).
t2 = CORRESPONDING #( t1 ).
WRITE / |[{ t2[ 1 ]-a }][{ t2[ 1 ]-b }]|.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("[abcde][01234567]");
  });

  it("MAPPING a sub structure to an elementary component", async () => {
    const code = `
TYPES: BEGIN OF ty_sub,
         a TYPE c LENGTH 5,
         b TYPE c LENGTH 10,
       END OF ty_sub.
TYPES: BEGIN OF ty1,
         s TYPE ty_sub,
       END OF ty1.
TYPES: BEGIN OF ty2,
         t TYPE c LENGTH 15,
       END OF ty2.
DATA s1 TYPE ty1.
DATA s2 TYPE ty2.
s1-s-a = 'abcde'.
s1-s-b = '0123456789'.
s2 = CORRESPONDING #( s1 MAPPING t = s ).
WRITE / |[{ s2-t }]|.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("[abcde0123456789]");
  });

  it("table, MAPPING of two components, others by name", async () => {
    const code = `
TYPES: BEGIN OF ty1,
         k TYPE i,
         v TYPE string,
         x TYPE i,
       END OF ty1.
TYPES: BEGIN OF ty2,
         key TYPE i,
         val TYPE string,
         x   TYPE i,
       END OF ty2.
TYPES ty_tab1 TYPE STANDARD TABLE OF ty1 WITH EMPTY KEY.
TYPES ty_tab2 TYPE STANDARD TABLE OF ty2 WITH EMPTY KEY.
DATA t1 TYPE ty_tab1.
DATA t2 TYPE ty_tab2.
DATA row TYPE ty2.
t1 = VALUE #( ( k = 1 v = |a| x = 7 ) ( k = 2 v = |b| x = 8 ) ).
t2 = CORRESPONDING #( t1 MAPPING key = k val = v ).
WRITE / lines( t2 ).
LOOP AT t2 INTO row.
  WRITE / |{ row-key } { row-val } { row-x }|.
ENDLOOP.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("2\n1 a 7\n2 b 8");
  });

  it("table, MAPPING onto the key of a sorted target", async () => {
    const code = `
TYPES: BEGIN OF ty1,
         k TYPE i,
       END OF ty1.
TYPES: BEGIN OF ty2,
         key TYPE i,
       END OF ty2.
TYPES ty_tab1 TYPE STANDARD TABLE OF ty1 WITH EMPTY KEY.
TYPES ty_tab2 TYPE SORTED TABLE OF ty2 WITH UNIQUE KEY key.
DATA t1 TYPE ty_tab1.
DATA t2 TYPE ty_tab2.
DATA row TYPE ty2.
t1 = VALUE #( ( k = 3 ) ( k = 1 ) ( k = 2 ) ).
t2 = CORRESPONDING #( t1 MAPPING key = k ).
LOOP AT t2 INTO row.
  WRITE / row-key.
ENDLOOP.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("1\n2\n3");
  });

  it("table, MAPPING with an empty source clears the target", async () => {
    const code = `
TYPES: BEGIN OF ty1,
         k TYPE i,
       END OF ty1.
TYPES: BEGIN OF ty2,
         key TYPE i,
       END OF ty2.
TYPES ty_tab1 TYPE STANDARD TABLE OF ty1 WITH EMPTY KEY.
TYPES ty_tab2 TYPE STANDARD TABLE OF ty2 WITH EMPTY KEY.
DATA t1 TYPE ty_tab1.
DATA t2 TYPE ty_tab2.
t2 = VALUE #( ( key = 1 ) ).
t2 = CORRESPONDING #( t1 MAPPING key = k ).
WRITE / lines( t2 ).`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("0");
  });

  it("table, MAPPING from a hashed source", async () => {
    const code = `
TYPES: BEGIN OF ty1,
         k TYPE i,
       END OF ty1.
TYPES: BEGIN OF ty2,
         key TYPE i,
       END OF ty2.
TYPES ty_tab1 TYPE HASHED TABLE OF ty1 WITH UNIQUE KEY k.
TYPES ty_tab2 TYPE STANDARD TABLE OF ty2 WITH EMPTY KEY.
DATA t1 TYPE ty_tab1.
DATA t2 TYPE ty_tab2.
t1 = VALUE #( ( k = 1 ) ( k = 2 ) ).
t2 = CORRESPONDING #( t1 MAPPING key = k ).
WRITE / lines( t2 ).`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("2");
  });

  it("table, BASE keeps the base rows and appends the source rows", async () => {
    const code = `
TYPES: BEGIN OF ty1,
         a TYPE i,
       END OF ty1.
TYPES: BEGIN OF ty2,
         a TYPE i,
         b TYPE i,
       END OF ty2.
DATA t1 TYPE STANDARD TABLE OF ty1 WITH EMPTY KEY.
DATA t2 TYPE STANDARD TABLE OF ty2 WITH EMPTY KEY.
DATA row TYPE ty2.
t1 = VALUE #( ( a = 1 ) ).
t2 = VALUE #( ( a = 5 b = 6 ) ).
t2 = CORRESPONDING #( BASE ( t2 ) t1 ).
LOOP AT t2 INTO row.
  WRITE / row-a.
  WRITE / row-b.
ENDLOOP.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("5\n6\n1\n0");
  });

  it("table, BASE with MAPPING, and BASE into the shorter row type", async () => {
    const code = `
TYPES: BEGIN OF ty1,
         a TYPE i,
       END OF ty1.
TYPES: BEGIN OF ty2,
         a TYPE i,
         b TYPE i,
       END OF ty2.
DATA t1 TYPE STANDARD TABLE OF ty1 WITH EMPTY KEY.
DATA t2 TYPE STANDARD TABLE OF ty2 WITH EMPTY KEY.
DATA row TYPE ty2.
t1 = VALUE #( ( a = 1 ) ).
t2 = VALUE #( ( a = 5 b = 6 ) ( a = 1 b = 0 ) ).
t2 = CORRESPONDING #( BASE ( t2 ) t1 MAPPING b = a ).
WRITE / lines( t2 ).
LOOP AT t2 INTO row.
  WRITE / row-a.
  WRITE / row-b.
ENDLOOP.
t1 = CORRESPONDING #( BASE ( t1 ) t2 ).
WRITE / lines( t1 ).`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("3\n5\n6\n1\n0\n1\n1\n4");
  });

  it("table, BASE into a sorted target", async () => {
    const code = `
TYPES: BEGIN OF ty1,
         a TYPE i,
         b TYPE i,
       END OF ty1.
TYPES: BEGIN OF ty2,
         a TYPE i,
         c TYPE i,
       END OF ty2.
DATA t1 TYPE STANDARD TABLE OF ty1 WITH EMPTY KEY.
DATA t2 TYPE SORTED TABLE OF ty2 WITH UNIQUE KEY a.
DATA row TYPE ty2.
t1 = VALUE #( ( a = 1 b = 2 ) ( a = 3 b = 4 ) ).
t2 = VALUE #( ( a = 9 c = 9 ) ).
t2 = CORRESPONDING #( BASE ( t2 ) t1 ).
LOOP AT t2 INTO row.
  WRITE / row-a.
  WRITE / row-c.
ENDLOOP.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("1\n0\n3\n0\n9\n9");
  });

  it("table into generic CHANGING parameter", async () => {
    const code = `
TYPES: BEGIN OF ty1,
         foo TYPE i,
         bar TYPE i,
       END OF ty1.
TYPES: BEGIN OF ty2,
         bar TYPE i,
         moo TYPE i,
       END OF ty2.
TYPES ty_tab1 TYPE STANDARD TABLE OF ty1 WITH EMPTY KEY.
TYPES ty_tab2 TYPE STANDARD TABLE OF ty2 WITH EMPTY KEY.

CLASS lcl DEFINITION.
  PUBLIC SECTION.
    CLASS-METHODS run IMPORTING src TYPE ty_tab1 CHANGING ct TYPE STANDARD TABLE.
ENDCLASS.

CLASS lcl IMPLEMENTATION.
  METHOD run.
    ct = CORRESPONDING #( src ).
  ENDMETHOD.
ENDCLASS.

START-OF-SELECTION.
  DATA t1 TYPE ty_tab1.
  DATA t2 TYPE ty_tab2.
  DATA row TYPE ty2.
  t1 = VALUE #( ( foo = 1 bar = 2 ) ( foo = 3 bar = 4 ) ).
  t2 = VALUE #( ( bar = 9 moo = 9 ) ).
  lcl=>run( EXPORTING src = t1 CHANGING ct = t2 ).
  WRITE / lines( t2 ).
  LOOP AT t2 INTO row.
    WRITE / |{ row-bar } { row-moo }|.
  ENDLOOP.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("2\n2 0\n4 0");
  });

  it("table into generic CHANGING parameter, MAPPING", async () => {
    const code = `
TYPES: BEGIN OF ty1,
         k TYPE i,
       END OF ty1.
TYPES: BEGIN OF ty2,
         key TYPE i,
       END OF ty2.
TYPES ty_tab1 TYPE STANDARD TABLE OF ty1 WITH EMPTY KEY.
TYPES ty_tab2 TYPE STANDARD TABLE OF ty2 WITH EMPTY KEY.

CLASS lcl DEFINITION.
  PUBLIC SECTION.
    CLASS-METHODS run IMPORTING src TYPE ty_tab1 CHANGING ct TYPE STANDARD TABLE.
ENDCLASS.

CLASS lcl IMPLEMENTATION.
  METHOD run.
    ct = CORRESPONDING #( src MAPPING key = k ).
  ENDMETHOD.
ENDCLASS.

START-OF-SELECTION.
  DATA t1 TYPE ty_tab1.
  DATA t2 TYPE ty_tab2.
  DATA row TYPE ty2.
  t1 = VALUE #( ( k = 5 ) ).
  lcl=>run( EXPORTING src = t1 CHANGING ct = t2 ).
  LOOP AT t2 INTO row.
    WRITE / row-key.
  ENDLOOP.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("5");
  });

  it("table into generic CHANGING parameter, BASE", async () => {
    const code = `
TYPES: BEGIN OF ty1,
         foo TYPE i,
       END OF ty1.
TYPES ty_tab1 TYPE STANDARD TABLE OF ty1 WITH EMPTY KEY.

CLASS lcl DEFINITION.
  PUBLIC SECTION.
    CLASS-METHODS run IMPORTING src TYPE ty_tab1 CHANGING ct TYPE STANDARD TABLE.
ENDCLASS.

CLASS lcl IMPLEMENTATION.
  METHOD run.
    ct = CORRESPONDING #( BASE ( ct ) src ).
  ENDMETHOD.
ENDCLASS.

START-OF-SELECTION.
  DATA t1 TYPE ty_tab1.
  DATA t2 TYPE ty_tab1.
  DATA row TYPE ty1.
  t1 = VALUE #( ( foo = 2 ) ).
  t2 = VALUE #( ( foo = 1 ) ).
  lcl=>run( EXPORTING src = t1 CHANGING ct = t2 ).
  LOOP AT t2 INTO row.
    WRITE / row-foo.
  ENDLOOP.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("1\n2");
  });

  it("structure into generic CHANGING parameter", async () => {
    const code = `
TYPES: BEGIN OF ty1,
         foo TYPE i,
         bar TYPE i,
       END OF ty1.
TYPES: BEGIN OF ty2,
         bar TYPE i,
         moo TYPE i,
       END OF ty2.

CLASS lcl DEFINITION.
  PUBLIC SECTION.
    CLASS-METHODS run IMPORTING src TYPE ty1 CHANGING cs TYPE any.
ENDCLASS.

CLASS lcl IMPLEMENTATION.
  METHOD run.
    cs = CORRESPONDING #( src ).
  ENDMETHOD.
ENDCLASS.

START-OF-SELECTION.
  DATA s1 TYPE ty1.
  DATA s2 TYPE ty2.
  s1-foo = 1.
  s1-bar = 2.
  s2-moo = 9.
  lcl=>run( EXPORTING src = s1 CHANGING cs = s2 ).
  WRITE / |{ s2-bar } { s2-moo }|.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("2 0");
  });

  it("table into generic field symbol", async () => {
    const code = `
TYPES: BEGIN OF ty1,
         foo TYPE i,
       END OF ty1.
TYPES ty_tab1 TYPE STANDARD TABLE OF ty1 WITH EMPTY KEY.
DATA t1 TYPE ty_tab1.
DATA t2 TYPE ty_tab1.
FIELD-SYMBOLS <tab> TYPE STANDARD TABLE.
t1 = VALUE #( ( foo = 3 ) ).
ASSIGN t2 TO <tab>.
<tab> = CORRESPONDING #( t1 ).
WRITE / lines( t2 ).`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("1");
  });

});
