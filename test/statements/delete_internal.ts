import {expect} from "chai";
import {ABAP, MemoryConsole} from "../../packages/runtime/src";
import {AsyncFunction, runFiles} from "../_utils";

let abap: ABAP;

async function run(contents: string) {
  return runFiles(abap, [{filename: "zfoobar.prog.abap", contents}]);
}

describe("Running statements - DELETE internal", () => {

  beforeEach(async () => {
    abap = new ABAP({console: new MemoryConsole()});
  });

  it("Basic delete internal", async () => {
    const code = `
      DATA table TYPE STANDARD TABLE OF i.
      APPEND 1 TO table.
      APPEND 2 TO table.
      DELETE table WHERE table_line = 1.
      ASSERT lines( table ) = 1.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
  });

  it("DELETE WHERE sets sy-subrc", async () => {
    const code = `
      DATA table TYPE STANDARD TABLE OF i.
      APPEND 1 TO table.
      APPEND 2 TO table.
      DELETE table WHERE table_line = 1.
      ASSERT sy-subrc = 0.
      DELETE table WHERE table_line = 1.
      ASSERT sy-subrc = 4.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
  });

  it("DELETE from table INDEX 1", async () => {
    const code = `
      DATA foo TYPE STANDARD TABLE OF i WITH DEFAULT KEY.
      APPEND 2 TO foo.
      APPEND 3 TO foo.
      DELETE foo INDEX 1.
      ASSERT lines( foo ) = 1.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
  });

  it("DELETE from table INDEX 2", async () => {
    const code = `
      DATA foo TYPE STANDARD TABLE OF i WITH DEFAULT KEY.
      APPEND 2 TO foo.
      APPEND 3 TO foo.
      DELETE foo INDEX 2.
      ASSERT lines( foo ) = 1.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
  });

  it("Basic delete ADJACENT DUPLICATES, no deleted", async () => {
    const code = `
      DATA table TYPE STANDARD TABLE OF i.
      APPEND 1 TO table.
      APPEND 2 TO table.
      DELETE ADJACENT DUPLICATES FROM table.
      ASSERT lines( table ) = 2.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
  });

  it("Basic delete ADJACENT DUPLICATES, one deleted", async () => {
    const code = `
      DATA table TYPE STANDARD TABLE OF i.
      APPEND 1 TO table.
      APPEND 2 TO table.
      APPEND 2 TO table.
      DELETE ADJACENT DUPLICATES FROM table.
      ASSERT lines( table ) = 2.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
  });

  it("DELETE table FROM index", async () => {
    const code = `
      DATA tab TYPE STANDARD TABLE OF i WITH DEFAULT KEY.
      DATA row LIKE LINE OF tab.
      DO 4 TIMES.
        APPEND sy-index TO tab.
      ENDDO.
      DELETE tab FROM 2.
      LOOP AT tab INTO row.
        WRITE / row.
      ENDLOOP.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("1");
  });

  it("DELETE table FROM index TO index", async () => {
    const code = `
TYPES integer_table TYPE STANDARD TABLE OF i WITH EMPTY KEY.
DATA numbers TYPE integer_table.
numbers = VALUE integer_table( ( 1 ) ( 2 ) ( 3 ) ).
DELETE numbers FROM 1 TO 2.
ASSERT lines( numbers ) = 1.
ASSERT numbers[ 1 ] = 3.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
  });

  it("DELETE INITIAL where IS INITIAL", async () => {
    const code = `
      DATA lt_keywords TYPE STANDARD TABLE OF string WITH DEFAULT KEY.
      APPEND '' TO lt_keywords.
      ASSERT lines( lt_keywords ) = 1.
      DELETE lt_keywords WHERE table_line IS INITIAL.
      ASSERT lines( lt_keywords ) = 0.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
  });

  it("delete internal tab with object references", async () => {
    const code = `
      CLASS lcl_foo DEFINITION.
      ENDCLASS.
      CLASS lcl_foo IMPLEMENTATION.
      ENDCLASS.

      DATA tab TYPE STANDARD TABLE OF REF TO lcl_foo.
      DATA ref1 TYPE REF TO lcl_foo.
      DATA ref2 TYPE REF TO lcl_foo.
      CREATE OBJECT ref1.
      APPEND ref1 TO tab.
      CREATE OBJECT ref2.
      APPEND ref2 TO tab.

      DELETE tab INDEX 2.
      ASSERT sy-subrc = 0.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
  });

  it("DELETE ADJACENT DUPLICATES COMPARING, 1", async () => {
    const code = `
      TYPES:
        BEGIN OF ty_foo,
          bar TYPE i,
          baz TYPE i,
        END OF ty_foo.
      DATA foo TYPE ty_foo.
      DATA footab TYPE TABLE OF ty_foo.
      DO 8 TIMES.
        foo-bar = sy-index DIV 2.
        foo-baz = sy-index MOD 4.
        APPEND foo TO footab.
      ENDDO.
      SORT footab BY baz.
      DELETE ADJACENT DUPLICATES FROM footab COMPARING baz.
      LOOP AT footab INTO foo.
        WRITE / |{ foo-bar }{ foo-baz }|.
      ENDLOOP.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("20\n01\n12\n13");
  });

  it("DELETE ADJACENT DUPLICATES COMPARING, 2", async () => {
    const code = `
      TYPES:
        BEGIN OF ty_foo,
          bar TYPE i,
          baz TYPE i,
        END OF ty_foo.
      DATA foo TYPE ty_foo.
      DATA footab TYPE TABLE OF ty_foo.
      DO 8 TIMES.
        foo-bar = sy-index MOD 2.
        foo-baz = sy-index DIV 4.
        APPEND foo TO footab.
      ENDDO.
      SORT footab BY bar.
      DELETE ADJACENT DUPLICATES FROM footab COMPARING bar baz.
      LOOP AT footab INTO foo.
        WRITE / |{ foo-bar }{ foo-baz }|.
      ENDLOOP.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("00\n01\n02\n10\n11");
  });

  it("DELETE ADJACENT DUPLICATES COMPARING ALL FIELDS", async () => {
    const code = `
      TYPES:
        BEGIN OF ty_foo,
          bar TYPE i,
          baz TYPE i,
        END OF ty_foo.
      DATA foo TYPE ty_foo.
      DATA footab TYPE TABLE OF ty_foo.
      DO 8 TIMES.
        foo-bar = sy-index MOD 2.
        foo-baz = sy-index DIV 4.
        APPEND foo TO footab.
      ENDDO.
      SORT footab BY bar.
      DELETE ADJACENT DUPLICATES FROM footab COMPARING ALL FIELDS.
      LOOP AT footab INTO foo.
        WRITE / |{ foo-bar }{ foo-baz }|.
      ENDLOOP.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("00\n01\n02\n10\n11");
  });

  it("DELETE ADJACENT DUPLICATES, default key is the character-like and byte-like components", async () => {
    const code = `
      TYPES:
        BEGIN OF ty_row,
          c    TYPE c LENGTH 2,
          n    TYPE n LENGTH 2,
          d    TYPE d,
          t    TYPE t,
          x    TYPE x LENGTH 1,
          str  TYPE string,
          xstr TYPE xstring,
          i    TYPE i,
          i8   TYPE int8,
          p    TYPE p LENGTH 8 DECIMALS 2,
          f    TYPE f,
          df   TYPE decfloat34,
        END OF ty_row.
      DATA tab TYPE STANDARD TABLE OF ty_row WITH DEFAULT KEY.
      DATA row TYPE ty_row.
      DATA names TYPE STANDARD TABLE OF string WITH DEFAULT KEY.
      DATA name TYPE string.
      FIELD-SYMBOLS <field> TYPE any.
      SPLIT 'C N D T X STR XSTR I I8 P F DF' AT space INTO TABLE names.
      LOOP AT names INTO name.
        CLEAR tab.
        CLEAR row.
        APPEND row TO tab.
        ASSIGN COMPONENT name OF STRUCTURE row TO <field>.
        <field> = '11'.
        APPEND row TO tab.
        DELETE ADJACENT DUPLICATES FROM tab.
        WRITE / |{ name } { lines( tab ) }|.
      ENDLOOP.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("C 2\nN 2\nD 2\nT 2\nX 2\nSTR 2\nXSTR 2\nI 1\nI8 1\nP 1\nF 1\nDF 1");
  });

  it("DELETE ADJACENT DUPLICATES, default key of nested structure", async () => {
    const code = `
      TYPES:
        BEGIN OF ty_sub,
          c TYPE c LENGTH 2,
          i TYPE i,
        END OF ty_sub.
      TYPES:
        BEGIN OF ty_row,
          k   TYPE c LENGTH 2,
          sub TYPE ty_sub,
          tab TYPE STANDARD TABLE OF i WITH DEFAULT KEY,
          ref TYPE REF TO i,
        END OF ty_row.
      DATA tab TYPE STANDARD TABLE OF ty_row WITH DEFAULT KEY.
      DATA row TYPE ty_row.
      DO 4 TIMES.
        CLEAR tab.
        CLEAR row.
        APPEND row TO tab.
        CASE sy-index.
          WHEN 1.
            row-sub-c = 'A'.
          WHEN 2.
            row-sub-i = 1.
          WHEN 3.
            APPEND 1 TO row-tab.
          WHEN 4.
            CREATE DATA row-ref.
        ENDCASE.
        APPEND row TO tab.
        DELETE ADJACENT DUPLICATES FROM tab.
        WRITE / lines( tab ).
      ENDDO.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("2\n1\n1\n1");
  });

  it("DELETE ADJACENT DUPLICATES, empty key deletes nothing", async () => {
    const code = `
      TYPES:
        BEGIN OF ty_row,
          i TYPE i,
          p TYPE p LENGTH 8 DECIMALS 2,
        END OF ty_row.
      DATA numeric TYPE STANDARD TABLE OF ty_row WITH DEFAULT KEY.
      DATA empty TYPE STANDARD TABLE OF ty_row WITH EMPTY KEY.
      DATA row TYPE ty_row.
      APPEND row TO numeric.
      APPEND row TO numeric.
      DELETE ADJACENT DUPLICATES FROM numeric.
      WRITE / lines( numeric ).
      APPEND row TO empty.
      APPEND row TO empty.
      DELETE ADJACENT DUPLICATES FROM empty.
      WRITE / lines( empty ).`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("2\n2");
  });

  it("DELETE ADJACENT DUPLICATES, table without key definition uses default key", async () => {
    const code = `
      TYPES:
        BEGIN OF ty_row,
          c TYPE c LENGTH 2,
          i TYPE i,
        END OF ty_row.
      DATA tab TYPE TABLE OF ty_row.
      DATA row TYPE ty_row.
      APPEND row TO tab.
      row-i = 7.
      APPEND row TO tab.
      DELETE ADJACENT DUPLICATES FROM tab.
      WRITE / lines( tab ).`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("1");
  });

  it("DELETE ADJACENT DUPLICATES, user defined key", async () => {
    const code = `
      TYPES:
        BEGIN OF ty_row,
          c TYPE c LENGTH 2,
          n TYPE n LENGTH 2,
          i TYPE i,
        END OF ty_row.
      DATA standard TYPE STANDARD TABLE OF ty_row WITH NON-UNIQUE KEY i.
      DATA sorted TYPE SORTED TABLE OF ty_row WITH NON-UNIQUE KEY c.
      DATA whole TYPE STANDARD TABLE OF ty_row WITH NON-UNIQUE KEY table_line.
      DATA row TYPE ty_row.
      APPEND row TO standard.
      row-c = 'AB'.
      APPEND row TO standard.
      row-i = 5.
      APPEND row TO standard.
      DELETE ADJACENT DUPLICATES FROM standard.
      WRITE / lines( standard ).
      CLEAR row.
      INSERT row INTO TABLE sorted.
      row-n = '01'.
      INSERT row INTO TABLE sorted.
      row-c = 'B'.
      INSERT row INTO TABLE sorted.
      DELETE ADJACENT DUPLICATES FROM sorted.
      WRITE / lines( sorted ).
      CLEAR row.
      APPEND row TO whole.
      row-i = 1.
      APPEND row TO whole.
      APPEND row TO whole.
      DELETE ADJACENT DUPLICATES FROM whole.
      WRITE / lines( whole ).`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("2\n2\n2");
  });

  it("DELETE ADJACENT DUPLICATES, DDIC table type with key table_line", async () => {
    const ttyp = `<?xml version="1.0" encoding="utf-8"?>
<abapGit version="v1.0.0" serializer="LCL_OBJECT_TTYP" serializer_version="v1.0.0">
 <asx:abap xmlns:asx="http://www.sap.com/abapxml" version="1.0">
  <asx:values>
   <DD40V>
    <TYPENAME>ZTTYP_ROW</TYPENAME>
    <DDLANGUAGE>E</DDLANGUAGE>
    <ROWTYPE>ZSROW</ROWTYPE>
    <ROWKIND>S</ROWKIND>
    <DATATYPE>STRU</DATATYPE>
    <ACCESSMODE>T</ACCESSMODE>
    <KEYDEF>T</KEYDEF>
    <KEYKIND>N</KEYKIND>
   </DD40V>
  </asx:values>
 </asx:abap>
</abapGit>`;
    const tabl = `<?xml version="1.0" encoding="utf-8"?>
<abapGit version="v1.0.0" serializer="LCL_OBJECT_TABL" serializer_version="v1.0.0">
 <asx:abap xmlns:asx="http://www.sap.com/abapxml" version="1.0">
  <asx:values>
   <DD02V>
    <TABNAME>ZSROW</TABNAME>
    <DDLANGUAGE>E</DDLANGUAGE>
    <TABCLASS>INTTAB</TABCLASS>
    <EXCLASS>1</EXCLASS>
   </DD02V>
   <DD03P_TABLE>
    <DD03P>
     <FIELDNAME>C</FIELDNAME>
     <INTTYPE>C</INTTYPE>
     <INTLEN>000004</INTLEN>
     <DATATYPE>CHAR</DATATYPE>
     <LENG>000002</LENG>
     <MASK>  CHAR</MASK>
    </DD03P>
    <DD03P>
     <FIELDNAME>I</FIELDNAME>
     <INTTYPE>X</INTTYPE>
     <INTLEN>000004</INTLEN>
     <DATATYPE>INT4</DATATYPE>
     <LENG>000010</LENG>
     <MASK>  INT4</MASK>
    </DD03P>
   </DD03P_TABLE>
  </asx:values>
 </asx:abap>
</abapGit>`;
    const code = `
      DATA tab TYPE zttyp_row.
      DATA row TYPE zsrow.
      APPEND row TO tab.
      row-i = 1.
      APPEND row TO tab.
      APPEND row TO tab.
      DELETE ADJACENT DUPLICATES FROM tab.
      WRITE / lines( tab ).`;
    const js = await runFiles(abap, [
      {filename: "zfoobar.prog.abap", contents: code},
      {filename: "zttyp_row.ttyp.xml", contents: ttyp},
      {filename: "zsrow.tabl.xml", contents: tabl}]);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("2");
  });

  it("DELETE WHERE method_call( ), check it compiles to valid JS", async () => {
    const code = `
CLASS lcl_bar DEFINITION.
  PUBLIC SECTION.
    METHODS run.
    METHODS get_selected_commit RETURNING VALUE(val) TYPE string.
ENDCLASS.

CLASS lcl_bar IMPLEMENTATION.
  METHOD run.
    TYPES: BEGIN OF ty_commit,
             sha1 TYPE string,
           END OF ty_commit.
    DATA lt_commits TYPE STANDARD TABLE OF ty_commit WITH DEFAULT KEY.
    DELETE lt_commits WHERE sha1 = get_selected_commit( ).
  ENDMETHOD.
  METHOD get_selected_commit.
  ENDMETHOD.
ENDCLASS.

START-OF-SELECTION.
  DATA bar TYPE REF TO lcl_bar.
  CREATE OBJECT bar.
  bar->run( ).`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
  });

  it("DELETE from table line", async () => {
    const code = `
DATA ignore TYPE STANDARD TABLE OF string WITH DEFAULT KEY.
DATA lv_name TYPE string.
APPEND 'foo' TO ignore.
lv_name = 'foo'.
DELETE TABLE ignore FROM lv_name.
ASSERT lines( ignore ) = 0.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
  });

  it("TO index", async () => {
    const code = `
DATA tab TYPE STANDARD TABLE OF i.
DATA int TYPE i.
DO 5 TIMES.
  APPEND sy-index TO tab.
ENDDO.
DELETE tab TO 3.
LOOP AT tab INTO int.
  WRITE / int.
ENDLOOP.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("4\n5");
  });

  it("DELETE fs INDEX", async () => {
    const code = `
DATA chain_tokens TYPE STANDARD TABLE OF string WITH DEFAULT KEY.
FIELD-SYMBOLS <tokens> TYPE STANDARD TABLE.
ASSIGN chain_tokens TO <tokens>.
DELETE <tokens> INDEX 1.
WRITE sy-subrc.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("4");
  });

  it("DELETE WHERE NOT IN", async () => {
    const code = `
DATA tab TYPE STANDARD TABLE OF i WITH DEFAULT KEY.
DATA bar TYPE RANGE OF i.
FIELD-SYMBOLS <moo> LIKE LINE OF bar.
APPEND INITIAL LINE TO bar ASSIGNING <moo>.
<moo>-sign = 'I'.
<moo>-option = 'EQ'.
<moo>-low = 2.

DO 4 TIMES.
  APPEND sy-index TO tab.
ENDDO.
DELETE tab WHERE table_line NOT IN bar.

ASSERT lines( tab ) = 1.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
  });

  it("DELETE WHERE IN", async () => {
    const code = `
DATA tab TYPE STANDARD TABLE OF i WITH DEFAULT KEY.
DATA bar TYPE RANGE OF i.
FIELD-SYMBOLS <moo> LIKE LINE OF bar.
APPEND INITIAL LINE TO bar ASSIGNING <moo>.
<moo>-sign = 'I'.
<moo>-option = 'EQ'.
<moo>-low = 2.

DO 4 TIMES.
  APPEND sy-index TO tab.
ENDDO.
DELETE tab WHERE table_line IN bar.

ASSERT lines( tab ) = 3.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
  });

  it("DELETE more IN", async () => {
    const code = `
DATA tab TYPE STANDARD TABLE OF i WITH DEFAULT KEY.
DATA bar TYPE RANGE OF i.
FIELD-SYMBOLS <moo> LIKE LINE OF bar.
APPEND INITIAL LINE TO bar ASSIGNING <moo>.
<moo>-sign = 'I'.
<moo>-option = 'EQ'.
<moo>-low = 2.
APPEND INITIAL LINE TO bar ASSIGNING <moo>.
<moo>-sign = 'I'.
<moo>-option = 'EQ'.
<moo>-low = 4.

DO 4 TIMES.
  APPEND sy-index TO tab.
ENDDO.
DELETE tab WHERE table_line NOT IN bar.

ASSERT lines( tab ) = 2.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
  });

  it("DELETE TO", async () => {
    const code = `
DATA lt_dists TYPE STANDARD TABLE OF i WITH DEFAULT KEY.
DATA lv_val LIKE LINE OF lt_dists.
DO 10 TIMES.
  APPEND sy-index TO lt_dists.
ENDDO.
DELETE lt_dists TO 5.
ASSERT lines( lt_dists ) = 5.
READ TABLE lt_dists INDEX 1 INTO lv_val.
WRITE lv_val.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("6");
  });

  it("DELETE HASHED WHERE", async () => {
    const code = `
TYPES: BEGIN OF ty,
         field1 TYPE c LENGTH 2,
         field2 TYPE c LENGTH 2,
       END OF ty.
DATA tab TYPE HASHED TABLE OF ty WITH UNIQUE KEY field1 field2.
DATA row LIKE LINE OF tab.

row-field1 = 'AA'.
INSERT row INTO TABLE tab.

DELETE tab WHERE field1 = 'AA'.

WRITE lines( tab ).`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("0");
  });

  it("DELETE, implicit index", async () => {
    const code = `
TYPES: BEGIN OF ty,
         field TYPE i,
       END OF ty.
DATA tab TYPE STANDARD TABLE OF ty WITH DEFAULT KEY.
DATA row LIKE LINE OF tab.

row-field = 6.
INSERT row INTO TABLE tab.
row-field = 3.
INSERT row INTO TABLE tab.
row-field = 10.
INSERT row INTO TABLE tab.

LOOP AT tab TRANSPORTING NO FIELDS WHERE field <= 5.
  DELETE tab.
  EXIT.
ENDLOOP.

LOOP AT tab INTO row.
  WRITE / row-field.
ENDLOOP.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("6\n10");
  });

  it("DELETE, implicit index, early exit", async () => {
    const code = `
TYPES: BEGIN OF ty,
         field TYPE i,
       END OF ty.
DATA tab TYPE STANDARD TABLE OF ty WITH DEFAULT KEY.
DATA row LIKE LINE OF tab.

row-field = 6.
INSERT row INTO TABLE tab.
row-field = 3.
INSERT row INTO TABLE tab.
row-field = 3.
INSERT row INTO TABLE tab.
row-field = 10.
INSERT row INTO TABLE tab.

LOOP AT tab TRANSPORTING NO FIELDS WHERE field <= 5.
  DELETE tab.
  EXIT.
ENDLOOP.

LOOP AT tab INTO row.
  WRITE / row-field.
ENDLOOP.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("6\n3\n10");
  });

  it("DELETE, messing with sy-tabix", async () => {
    const code = `
TYPES: BEGIN OF ty,
         field TYPE i,
       END OF ty.
DATA tab TYPE STANDARD TABLE OF ty WITH DEFAULT KEY.
DATA row LIKE LINE OF tab.

row-field = 6.
INSERT row INTO TABLE tab.
row-field = 3.
INSERT row INTO TABLE tab.
row-field = 3.
INSERT row INTO TABLE tab.
row-field = 10.
INSERT row INTO TABLE tab.

LOOP AT tab TRANSPORTING NO FIELDS WHERE field <= 5.
  sy-tabix = 1.
  DELETE tab.
  EXIT.
ENDLOOP.

LOOP AT tab INTO row.
  WRITE / row-field.
ENDLOOP.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("6\n3\n10");
  });

  it("DELETE, with table key", async () => {
    const code = `
TYPES: BEGIN OF ty,
         foobar TYPE i,
       END OF ty.
TYPES ty_tt TYPE HASHED TABLE OF ty WITH UNIQUE KEY foobar.

DATA itab TYPE ty_tt.
DATA row LIKE LINE OF itab.

row-foobar = 1.
INSERT row INTO TABLE itab.
row-foobar = 2.
INSERT row INTO TABLE itab.
row-foobar = 3.
INSERT row INTO TABLE itab.

DELETE TABLE itab WITH TABLE KEY foobar = 2.

LOOP AT itab INTO row.
  WRITE / row-foobar.
ENDLOOP.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("1\n3");
  });

  it("DELETE, with awaited statement", async () => {
    const code = `
CLASS lcl DEFINITION.
  PUBLIC SECTION.
    METHODS get_column_index RETURNING VALUE(index) TYPE i.
    METHODS foo.
ENDCLASS.

CLASS lcl IMPLEMENTATION.
  METHOD get_column_index.
  ENDMETHOD.

  METHOD foo.
    TYPES:
      BEGIN OF mty_s_hashed_column,
        column_index TYPE i,
        column       TYPE REF TO object,
      END OF mty_s_hashed_column,
      mty_ts_hashed_column TYPE HASHED TABLE OF mty_s_hashed_column WITH UNIQUE KEY column_index.

    DATA columns_hashed TYPE mty_ts_hashed_column.

    DELETE TABLE columns_hashed WITH TABLE KEY column_index = get_column_index( ).
  ENDMETHOD.
ENDCLASS.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    // just test its valid syntax
  });

});