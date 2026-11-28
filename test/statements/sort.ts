import {expect} from "chai";
import {ABAP, MemoryConsole} from "../../packages/runtime/src";
import {AsyncFunction, runFiles} from "../_utils";

let abap: ABAP;

async function run(contents: string) {
  return runFiles(abap, [{filename: "zfoobar.prog.abap", contents}]);
}

describe("Running statements - SORT", () => {

  beforeEach(async () => {
    abap = new ABAP({console: new MemoryConsole()});
  });

  it("Basic sort table", async () => {
    const code = `
      DATA: table   TYPE STANDARD TABLE OF i,
            integer TYPE i.
      APPEND 2 TO table.
      APPEND 1 TO table.
      SORT table.
      LOOP AT table INTO integer.
        WRITE / integer.
      ENDLOOP.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("1\n2");
  });

  it("Basic sort table, descending", async() => {
    const code = `
      DATA: table   TYPE STANDARD TABLE OF i,
            integer TYPE i.
      APPEND 2 TO table.
      APPEND 3 TO table.
      APPEND 1 TO table.
      SORT table DESCENDING.
      LOOP AT table INTO integer.
        WRITE / integer.
      ENDLOOP.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("3\n2\n1");
  });

  it("SORT structure", async () => {
    const code = `
      TYPES: BEGIN OF ty_structure,
              field TYPE i,
            END OF ty_structure.
      DATA tab TYPE STANDARD TABLE OF ty_structure WITH DEFAULT KEY.
      DATA row LIKE LINE OF tab.
      row-field = 2.
      APPEND row TO tab.
      row-field = 1.
      APPEND row TO tab.
      SORT tab BY field.
      LOOP AT tab INTO row.
        WRITE / row-field.
      ENDLOOP.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("1\n2");
  });

  it("SORT BY table_line", async () => {
    const code = `
      DATA lt_keywords TYPE STANDARD TABLE OF string WITH DEFAULT KEY.
      APPEND 'foo' TO lt_keywords.
      APPEND 'bar' TO lt_keywords.
      SORT lt_keywords BY table_line ASCENDING.
      DATA keyword TYPE string.
      LOOP AT lt_keywords INTO keyword.
        WRITE / keyword.
      ENDLOOP.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("bar\nfoo");
  });

  it("SORT BY private", async () => {
    const code = `
CLASS lcl DEFINITION.
  PUBLIC SECTION.
    CLASS-METHODS run.
  PRIVATE SECTION.
    DATA foo TYPE i.
ENDCLASS.

CLASS lcl IMPLEMENTATION.
  METHOD run.
    DATA tab TYPE STANDARD TABLE OF REF TO lcl.
    DATA ref TYPE REF TO lcl.
    CREATE OBJECT ref.
    INSERT ref INTO TABLE tab.
    INSERT ref INTO TABLE tab.
    SORT tab BY table_line->foo.
  ENDMETHOD.
ENDCLASS.

START-OF-SELECTION.
  lcl=>run( ).`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
  });

  it("SORT BY interfaced var", async () => {
    const code = `
INTERFACE lif.
  DATA foo TYPE i.
ENDINTERFACE.

CLASS lcl DEFINITION.
  PUBLIC SECTION.
    INTERFACES lif.
    CLASS-METHODS run.
ENDCLASS.

CLASS lcl IMPLEMENTATION.
  METHOD run.
    DATA tab TYPE STANDARD TABLE OF REF TO lif.
    DATA ref TYPE REF TO lcl.
    CREATE OBJECT ref.
    INSERT ref INTO TABLE tab.
    INSERT ref INTO TABLE tab.
    SORT tab BY table_line->foo.
  ENDMETHOD.
ENDCLASS.

START-OF-SELECTION.
  lcl=>run( ).`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
  });

  it("SORT without BY, structure WITH DEFAULT KEY, sorts by the character-like components", async () => {
    const code = `
TYPES: BEGIN OF ty_row,
         name TYPE c LENGTH 2,
         n    TYPE i,
         s    TYPE string,
       END OF ty_row.
DATA lt_r TYPE STANDARD TABLE OF ty_row WITH DEFAULT KEY.
DATA ls TYPE ty_row.
DATA lv_out TYPE string.
ls-name = 'b'. ls-n = 1. ls-s = \`x\`. APPEND ls TO lt_r.
ls-name = 'a'. ls-n = 3. ls-s = \`y\`. APPEND ls TO lt_r.
ls-name = 'a'. ls-n = 4. ls-s = \`y\`. APPEND ls TO lt_r.
ls-name = 'a'. ls-n = 5. ls-s = \`y\`. APPEND ls TO lt_r.
ls-name = 'a'. ls-n = 9. ls-s = \`a\`. APPEND ls TO lt_r.
ls-name = 'A'. ls-n = 2. ls-s = \`z\`. APPEND ls TO lt_r.
SORT lt_r.
LOOP AT lt_r INTO ls.
  lv_out = lv_out && |{ ls-name }{ ls-n }{ ls-s };|.
ENDLOOP.
WRITE / lv_out.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("A2z;a9a;a3y;a4y;a5y;b1x;");
  });


  it("SORT BY (name), dynamic component, ascending and descending", async () => {
    const code = `
TYPES: BEGIN OF ty,
         a TYPE i,
         b TYPE string,
       END OF ty.
DATA lt TYPE STANDARD TABLE OF ty WITH EMPTY KEY.
DATA ls LIKE LINE OF lt.
DATA lv_name TYPE string.
ls-a = 1. ls-b = \`y\`. APPEND ls TO lt.
ls-a = 3. ls-b = \`x\`. APPEND ls TO lt.
ls-a = 2. ls-b = \`z\`. APPEND ls TO lt.
lv_name = 'A'.
SORT lt BY (lv_name).
LOOP AT lt INTO ls.
  WRITE / ls-a.
ENDLOOP.
SORT lt BY (lv_name) DESCENDING.
LOOP AT lt INTO ls.
  WRITE / ls-a.
ENDLOOP.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("1\n2\n3\n3\n2\n1");
  });

  it("SORT BY (name), literal, and a name with trailing blanks", async () => {
    const code = `
TYPES: BEGIN OF ty,
         a TYPE i,
         b TYPE string,
       END OF ty.
DATA lt TYPE STANDARD TABLE OF ty WITH EMPTY KEY.
DATA ls LIKE LINE OF lt.
DATA lv_name TYPE c LENGTH 10.
ls-a = 1. ls-b = \`y\`. APPEND ls TO lt.
ls-a = 3. ls-b = \`x\`. APPEND ls TO lt.
ls-a = 2. ls-b = \`z\`. APPEND ls TO lt.
SORT lt BY ('B').
LOOP AT lt INTO ls.
  WRITE / ls-a.
ENDLOOP.
lv_name = 'A'.
SORT lt BY (lv_name).
LOOP AT lt INTO ls.
  WRITE / ls-a.
ENDLOOP.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("3\n1\n2\n1\n2\n3");
  });

  it("SORT BY (name), dynamic and static component mixed", async () => {
    const code = `
TYPES: BEGIN OF ty,
         a TYPE i,
         b TYPE i,
       END OF ty.
DATA lt TYPE STANDARD TABLE OF ty WITH EMPTY KEY.
DATA ls LIKE LINE OF lt.
DATA lv_name TYPE string VALUE 'B'.
ls-a = 1. ls-b = 1. APPEND ls TO lt.
ls-a = 2. ls-b = 2. APPEND ls TO lt.
ls-a = 1. ls-b = 2. APPEND ls TO lt.
ls-a = 2. ls-b = 1. APPEND ls TO lt.
SORT lt BY a DESCENDING (lv_name).
LOOP AT lt INTO ls.
  WRITE / |{ ls-a }{ ls-b }|.
ENDLOOP.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("21\n22\n11\n12");
  });

  it("SORT BY table_line AS TEXT, umlauts, accents, case, digits and punctuation", async () => {
    const code = `
DATA tab TYPE STANDARD TABLE OF string WITH EMPTY KEY.
DATA s TYPE string.
DATA out TYPE string.
tab = VALUE #( ( |b| ) ( |B| ) ( |a| ) ( |A| ) ( |Z| ) ( |ä| ) ( |Ä| ) ( |ae| ) ( |af| ) ( |ad| )
  ( |ß| ) ( |ss| ) ( |st| ) ( |é| ) ( |e| ) ( |E| ) ( |è| ) ( |ê| ) ( |f| ) ( |1| ) ( |10| ) ( |2| )
  ( | a| ) ( |a b| ) ( |ab| ) ( |a-b| ) ( |a_b| ) ( |a.b| ) ( |a,b| ) ( |-| ) ( |_| ) ( |.| ) ( || )
  ( |ö| ) ( |o| ) ( |p| ) ( |ü| ) ( |u| ) ( |v| ) ( |ø| ) ( |å| ) ( |z| ) ( |ñ| ) ( |n| )
  ( |@| ) ( |#| ) ( |'| ) ( |a'b| ) ( |aB| ) ( |Ab| ) ( |AB| ) ).
SORT tab BY table_line AS TEXT.
LOOP AT tab INTO s.
  out = out && |[{ s }]|.
ENDLOOP.
ASSERT out = |[][ a][_][-][.]['][@][#][1][10][2][a][A][å][ä][Ä][a b][a_b][a-b][a,b][a.b][a'b][ab][aB][Ab][AB]|
  && |[ad][ae][af][b][B][e][E][é][è][ê][f][n][ñ][o][ö][ø][p][ss][ß][st][u][ü][v][z][Z]|.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
  });

  it("SORT DESCENDING AS TEXT, table of strings", async () => {
    const code = `
DATA tab TYPE STANDARD TABLE OF string WITH DEFAULT KEY.
DATA s TYPE string.
DATA out TYPE string.
tab = VALUE #( ( |b| ) ( |B| ) ( |a| ) ( |A| ) ( |Z| ) ( |ä| ) ).
SORT tab DESCENDING AS TEXT.
LOOP AT tab INTO s.
  out = out && s.
ENDLOOP.
ASSERT out = |ZBbäAa|.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
  });

  it("SORT AS TEXT without BY, character-like components of the default key", async () => {
    const code = `
TYPES: BEGIN OF ty,
         k TYPE c LENGTH 3,
         s TYPE string,
         n TYPE i,
       END OF ty.
DATA tab TYPE STANDARD TABLE OF ty WITH DEFAULT KEY.
DATA row TYPE ty.
DATA out TYPE string.
tab = VALUE #( ( k = 'b' s = |x| n = 2 ) ( k = 'B' s = |y| n = 1 ) ( k = 'a' s = |Y| n = 3 )
  ( k = 'ä' s = |x| n = 1 ) ( k = 'A' s = |x| n = 2 ) ( k = 'a' s = |y| n = 1 ) ).
SORT tab AS TEXT.
LOOP AT tab INTO row.
  out = out && row-k && row-s && row-n && |,|.
ENDLOOP.
ASSERT out = |ay1,aY3,Ax2,äx1,bx2,By1,|.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
  });

  it("SORT AS TEXT, per component and for the whole SORT", async () => {
    const code = `
TYPES: BEGIN OF ty,
         k TYPE c LENGTH 3,
         s TYPE string,
         n TYPE i,
       END OF ty.
TYPES tt TYPE STANDARD TABLE OF ty WITH EMPTY KEY.
DATA tab TYPE tt.
DATA sorted TYPE tt.
DATA row TYPE ty.
DATA out TYPE string.
tab = VALUE #( ( k = 'b' s = |x| n = 2 ) ( k = 'B' s = |y| n = 1 ) ( k = 'a' s = |Y| n = 3 )
  ( k = 'ä' s = |x| n = 1 ) ( k = 'A' s = |x| n = 2 ) ( k = 'a' s = |y| n = 1 ) ).

sorted = tab.
SORT sorted BY k DESCENDING AS TEXT s AS TEXT.
CLEAR out.
LOOP AT sorted INTO row.
  out = out && row-k && row-s && row-n && |,|.
ENDLOOP.
ASSERT out = |By1,bx2,äx1,Ax2,ay1,aY3,|.

sorted = tab.
SORT sorted DESCENDING AS TEXT BY k s.
CLEAR out.
LOOP AT sorted INTO row.
  out = out && row-k && row-s && row-n && |,|.
ENDLOOP.
ASSERT out = |By1,bx2,äx1,Ax2,aY3,ay1,|.

sorted = tab.
SORT sorted AS TEXT BY k s DESCENDING.
CLEAR out.
LOOP AT sorted INTO row.
  out = out && row-k && row-s && row-n && |,|.
ENDLOOP.
ASSERT out = |aY3,ay1,Ax2,äx1,bx2,By1,|.

sorted = tab.
SORT sorted BY k s.
CLEAR out.
LOOP AT sorted INTO row.
  out = out && row-k && row-s && row-n && |,|.
ENDLOOP.
ASSERT out = |Ax2,By1,aY3,ay1,bx2,äx1,|.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
  });

  it("SORT DESCENDING BY, the order of the SORT applies to components without their own", async () => {
    const code = `
TYPES: BEGIN OF ty,
         k TYPE c LENGTH 3,
         n TYPE i,
       END OF ty.
TYPES tt TYPE STANDARD TABLE OF ty WITH EMPTY KEY.
DATA tab TYPE tt.
DATA row TYPE ty.
DATA out TYPE string.
tab = VALUE #( ( k = 'a' n = 1 ) ( k = 'b' n = 2 ) ( k = 'a' n = 3 ) ).
SORT tab DESCENDING BY k n.
LOOP AT tab INTO row.
  out = out && row-k && row-n.
ENDLOOP.
ASSERT out = |b2a3a1|.

tab = VALUE #( ( k = 'a' n = 1 ) ( k = 'b' n = 2 ) ( k = 'a' n = 3 ) ).
SORT tab DESCENDING BY k n ASCENDING.
CLEAR out.
LOOP AT tab INTO row.
  out = out && row-k && row-n.
ENDLOOP.
ASSERT out = |b2a1a3|.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
  });

  it("SORT without BY, a table WITH EMPTY KEY keeps its order", async () => {
    const code = `
TYPES: BEGIN OF ty, k TYPE c LENGTH 1, v TYPE i, END OF ty.
DATA tab TYPE STANDARD TABLE OF ty WITH EMPTY KEY.
DATA def TYPE STANDARD TABLE OF ty WITH DEFAULT KEY.
DATA strings TYPE STANDARD TABLE OF string WITH EMPTY KEY.
DATA out TYPE string.

tab = VALUE #( ( k = 'b' v = 1 ) ( k = 'c' v = 2 ) ( k = 'a' v = 3 ) ).
SORT tab.
CLEAR out.
LOOP AT tab INTO DATA(l1).
  out = out && l1-k.
ENDLOOP.
WRITE / out.

SORT tab DESCENDING.
CLEAR out.
LOOP AT tab INTO DATA(l2).
  out = out && l2-k.
ENDLOOP.
WRITE / out.

SORT tab BY k.
CLEAR out.
LOOP AT tab INTO DATA(l3).
  out = out && l3-k.
ENDLOOP.
WRITE / out.

strings = VALUE #( ( \`b\` ) ( \`c\` ) ( \`a\` ) ).
SORT strings.
CLEAR out.
LOOP AT strings INTO DATA(l4).
  out = out && l4.
ENDLOOP.
WRITE / out.

def = VALUE #( ( k = 'b' v = 1 ) ( k = 'c' v = 2 ) ( k = 'a' v = 3 ) ).
SORT def.
CLEAR out.
LOOP AT def INTO DATA(l5).
  out = out && l5-k.
ENDLOOP.
WRITE / out.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.getTrimmed()).to.equal("bca\nbca\nabc\nbca\nabc");
  });

});
