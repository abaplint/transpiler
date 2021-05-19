import {expect} from "chai";
import {ABAP} from "../../packages/runtime/src";
import {AsyncFunction, runFiles} from "../_utils";

let abap: ABAP;

async function run(contents: string) {
  return runFiles(abap, [{filename: "zfoobar.prog.abap", contents}]);
}

describe("Running statements - LOOP", () => {

  beforeEach(async () => {
    abap = new ABAP();
  });

  it("LOOP AT pass by value", async () => {
    const code = `
      data tab type table of i.
      data val type i.
      append 2 to tab.
      loop at tab into val.
      write / val.
      val = 1.
      endloop.
      loop at tab into val.
      write / val.
      endloop.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("2\n2");
  });

  it("LOOP at assigning", async () => {
    const code = `
    DATA tab TYPE STANDARD TABLE OF i WITH DEFAULT KEY.
    FIELD-SYMBOLS <row> TYPE i.
    APPEND 1 TO tab.
    LOOP AT tab ASSIGNING <row>.
      <row> = 2.
    ENDLOOP.
    LOOP AT tab ASSIGNING <row>.
      WRITE <row>.
    ENDLOOP.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("2");
  });

  it("LOOP, should set sy-tabix", async () => {
    const code = `
      DATA tab TYPE STANDARD TABLE OF i WITH DEFAULT KEY.
      DO 3 TIMES.
        APPEND 'a' TO tab.
      ENDDO.
      DATA sdf TYPE i.
      LOOP AT tab INTO sdf.
        WRITE / sy-tabix.
      ENDLOOP.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("1\n2\n3");
  });

  it("LOOP AT WHERE", async () => {
    const code = `
      DATA tab TYPE STANDARD TABLE OF i WITH DEFAULT KEY.
      DATA line LIKE LINE OF tab.
      DO 5 TIMES.
        APPEND sy-index TO tab.
      ENDDO.
      LOOP AT tab INTO line WHERE table_line <> 3.
        WRITE / line.
      ENDLOOP.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("1\n2\n4\n5");
  });

  it("LOOP AT nothing with CONTINUE and EXIT", async () => {
    const code = `
      DATA tab TYPE STANDARD TABLE OF i WITH DEFAULT KEY.
      DATA row LIKE LINE OF tab.
      LOOP AT tab INTO row.
        CONTINUE.
      ENDLOOP.
      LOOP AT tab INTO row.
        EXIT.
      ENDLOOP.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
  });

  it("LOOPing and DELETE in same table", async () => {
    const code = `
      DATA tab TYPE STANDARD TABLE OF i WITH DEFAULT KEY.
      DATA row LIKE LINE OF tab.
      DO 4 TIMES.
        APPEND sy-index TO tab.
      ENDDO.
      ASSERT lines( tab ) = 4.
      LOOP AT tab INTO row.
        WRITE / sy-tabix.
        WRITE / row.
        DELETE tab INDEX 2.
      ENDLOOP.
      ASSERT lines( tab ) = 1.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("1\n1\n2\n3\n2\n4");
  });

  it("LOOPing FROM and TO", async () => {
    const code = `
      DATA tab TYPE STANDARD TABLE OF i.
      DATA line TYPE i.
      DO 10 TIMES.
        APPEND sy-index TO tab.
      ENDDO.
      LOOP AT tab FROM 8 INTO line.
        WRITE line.
      ENDLOOP.
      WRITE / ''.
      LOOP AT tab TO 3 INTO line.
        WRITE line.
      ENDLOOP.
      WRITE / ''.
      LOOP AT tab FROM 5 TO 6 INTO line.
        WRITE line.
      ENDLOOP.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("8910\n123\n56");
  });

  it("LOOPing FROM and TO with var and fs", async () => {
    const code = `
      DATA tab TYPE STANDARD TABLE OF i.
      DATA line TYPE i.
      DATA from TYPE i.
      FIELD-SYMBOLS <to> TYPE i.
      DO 10 TIMES.
        APPEND sy-index TO tab.
      ENDDO.
      from = 4.
      ASSIGN 7 TO <to>.
      LOOP AT tab FROM from TO <to> INTO line.
        WRITE line.
      ENDLOOP.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("4567");
  });

  it("LOOPing FROM and TO and WHERE", async () => {
    const code = `
      TYPES:
        BEGIN OF ty_struct,
          foo TYPE i,
          bar TYPE i,
        END OF ty_struct.
      DATA tab TYPE STANDARD TABLE OF ty_struct.
      DATA line TYPE ty_struct.
      DO 10 TIMES.
        line-foo = sy-index.
        line-bar = sy-index MOD 3.
        APPEND line TO tab.
        line-foo = sy-index * 3.
        line-bar = line-foo MOD 7 + 1.
        APPEND line TO tab.
      ENDDO.
      SORT tab BY foo bar.
      LOOP AT tab FROM 7 TO 13 INTO line WHERE foo < 8.
        WRITE |{ line-foo }-{ line-bar }.|.
      ENDLOOP.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("6-0.6-7.7-1.");
  });

  it("LOOPing FROM and TO, out of bounds", async () => {
    const code = `
      DATA tab TYPE STANDARD TABLE OF i.
      DATA line TYPE i.
      DO 5 TIMES.
        APPEND sy-index TO tab.
      ENDDO.
      LOOP AT tab FROM -3 TO 17 INTO line.
        WRITE line.
      ENDLOOP.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("12345");
  });

  it("LOOP AT <fs1> ASSIGNING <fs2>", async () => {
    const code = `
      TYPES:
        BEGIN OF ty_tabtab,
          index TYPE i,
          str_tab TYPE TABLE OF string,
        END OF ty_tabtab.
      DATA tabtab TYPE ty_tabtab.
      DATA foo TYPE TABLE OF ty_tabtab.
      tabtab-index = 1.
      APPEND 'foo' TO tabtab-str_tab.
      APPEND 'bar' TO tabtab-str_tab.
      APPEND tabtab TO foo.
      tabtab-index = 3.
      CLEAR tabtab-str_tab[].
      APPEND 'meh' TO tabtab-str_tab.
      APPEND tabtab TO foo.
      FIELD-SYMBOLS <foo> TYPE ty_tabtab.
      FIELD-SYMBOLS <bar> TYPE string.
      LOOP AT foo ASSIGNING <foo>.
        LOOP AT <foo>-str_tab ASSIGNING <bar>.
          WRITE <bar>.
        ENDLOOP.
      ENDLOOP.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("foobarmeh");
  });

  it("LOOP with multiple conditions", async () => {
    const code = `
      TYPES:
        BEGIN OF ty_struct,
          foo TYPE i,
          bar TYPE i,
        END OF ty_struct.
      DATA tab TYPE STANDARD TABLE OF ty_struct.
      DATA line TYPE ty_struct.
      DO 10 TIMES.
        line-foo = sy-index.
        line-bar = sy-index MOD 3.
        APPEND line TO tab.
        line-foo = sy-index * 3.
        line-bar = line-foo MOD 7 + 1.
        APPEND line TO tab.
      ENDDO.
      SORT tab BY foo bar.
      LOOP AT tab INTO line WHERE foo < 10 AND bar >= 2.
        WRITE |{ line-foo }-{ line-bar }.|.
      ENDLOOP.
      WRITE / ''.
      LOOP AT tab INTO line WHERE foo > 25 OR bar = 7.
        WRITE |{ line-foo }-{ line-bar }.|.
      ENDLOOP.
      WRITE / ''.
      LOOP AT tab INTO line WHERE foo = 18 OR ( foo > 23 AND foo <= 30 ).
        WRITE |{ line-foo }-{ line-bar }.|.
      ENDLOOP.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("2-2.3-4.5-2.6-7.8-2.9-3.\n6-7.27-7.30-3.\n18-5.24-4.27-7.30-3.");
  });

  it.skip("LOOP should set sy-subrc", async () => {
    const code = `
    DATA tab TYPE STANDARD TABLE OF i WITH DEFAULT KEY.
    APPEND 2 TO tab.
    LOOP AT tab TRANSPORTING NO FIELDS WHERE table_line = 1.
    ENDLOOP.
    WRITE / sy-subrc.
    LOOP AT tab TRANSPORTING NO FIELDS WHERE table_line = 2.
    ENDLOOP.
    WRITE / sy-subrc.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("4\n0");
  });

  it("LOOP, REFERENCE INTO", async () => {
    const code = `
TYPES: BEGIN OF ty_node,
         value TYPE i,
       END OF ty_node.
DATA tab TYPE STANDARD TABLE OF ty_node WITH DEFAULT KEY.
DATA row LIKE LINE OF tab.
DATA ref TYPE REF TO ty_node.

APPEND row TO tab.

LOOP AT tab REFERENCE INTO ref.
  ref->value = 4.
ENDLOOP.
LOOP AT tab REFERENCE INTO ref.
  WRITE ref->value.
ENDLOOP.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("4");
  });

  it("LOOP and APPEND INITIl ASSIGNING", async () => {
    const code = `
  DATA tab TYPE STANDARD TABLE OF i WITH DEFAULT KEY.
  FIELD-SYMBOLS <row> LIKE LINE OF tab.
  APPEND INITIAL LINE TO tab ASSIGNING <row>.
  <row> = 1.
  APPEND INITIAL LINE TO tab ASSIGNING <row>.
  <row> = 2.
  LOOP AT tab ASSIGNING <row>.
    WRITE / <row>.
  ENDLOOP.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("1\n2");
  });

  it("LOOP refs", async () => {
    const code = `
TYPES: BEGIN OF ty_data,
         field TYPE i,
       END OF ty_data.
DATA data TYPE ty_data.
DATA ref TYPE REF TO ty_data.
DATA tab TYPE STANDARD TABLE OF REF TO ty_data.

GET REFERENCE OF data INTO ref.
INSERT ref INTO tab INDEX 1.
ref->field = 2.

LOOP AT tab INTO ref.
  WRITE ref->field.
ENDLOOP.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("2");
  });

  it("LOOP refs", async () => {
    const code = `
TYPES: BEGIN OF ty_data,
         field TYPE i,
       END OF ty_data.
DATA data TYPE ty_data.
DATA ref TYPE REF TO ty_data.
DATA tab TYPE STANDARD TABLE OF REF TO ty_data.

GET REFERENCE OF data INTO ref.
INSERT ref INTO tab INDEX 1.
ref->field = 2.

LOOP AT tab INTO ref.
  ref->field = 3.
ENDLOOP.

LOOP AT tab INTO ref.
  WRITE ref->field.
ENDLOOP.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("3");
  });

});