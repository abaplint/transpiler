import {expect} from "chai";
import {AsyncFunction, runFiles} from "./_utils";
import {ABAP} from "../packages/runtime/src/";
import {msag_escape, msag_zag_unit_test, tabl_t100xml} from "./_data";

describe("Top level tests, Database", () => {
  let abap: ABAP;

  beforeEach(async () => {
    abap = new ABAP();
  });

  it("SELECT", async () => {
    const code = `
    DATA ls_result TYPE t100.
    SELECT SINGLE * FROM t100 INTO ls_result.
    WRITE ls_result-text.`;
    const js = await runFiles(abap, [
      {filename: "zfoobar.prog.abap", contents: code},
      {filename: "t100.tabl.xml", contents: tabl_t100xml},
      {filename: "zag_unit_test.msag.xml", contents: msag_zag_unit_test}]);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("hello world");
  });

  it("SELECT, no result", async () => {
    const code = `
    DATA ls_result TYPE t100.
    SELECT SINGLE * FROM t100 INTO ls_result.
    WRITE sy-subrc.`;
    const js = await runFiles(abap, [
      {filename: "zfoobar.prog.abap", contents: code},
      {filename: "t100.tabl.xml", contents: tabl_t100xml}]);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("4");
  });

  it("MODIFY FROM, inserts row", async () => {
    const code = `
    DATA tab TYPE STANDARD TABLE OF t100 WITH DEFAULT KEY.
    DATA row LIKE LINE OF tab.
    row-arbgb = 'HELLO'.
    APPEND row TO tab.
    MODIFY t100 FROM TABLE tab.
    SELECT SINGLE * FROM t100 INTO row.
    WRITE / sy-subrc.
    WRITE / row-arbgb.`;
    const js = await runFiles(abap, [
      {filename: "zfoobar.prog.abap", contents: code},
      {filename: "t100.tabl.xml", contents: tabl_t100xml}]);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("0\nHELLO");
  });

  it("MODIFY FROM, inserts and update", async () => {
    const code = `
    DATA tab TYPE STANDARD TABLE OF t100 WITH DEFAULT KEY.
    DATA row LIKE LINE OF tab.

    row-arbgb = 'HELLO'.
    APPEND row TO tab.
    MODIFY t100 FROM TABLE tab.

    CLEAR tab.

    row-arbgb = 'HELLO'.
    row-text = 'WORLD'.
    APPEND row TO tab.
    MODIFY t100 FROM TABLE tab.

    SELECT SINGLE * FROM t100 INTO row.
    WRITE / row-text.`;
    const js = await runFiles(abap, [
      {filename: "zfoobar.prog.abap", contents: code},
      {filename: "t100.tabl.xml", contents: tabl_t100xml}]);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    const cons = abap.console.get();
    expect(cons).to.equal("WORLD");
  });

  it("test, DELETE", async () => {
    const code = `
    DATA tab TYPE STANDARD TABLE OF t100 WITH DEFAULT KEY.
    DATA row LIKE LINE OF tab.

    row-arbgb = 'HELLO'.
    APPEND row TO tab.
    MODIFY t100 FROM TABLE tab.

    DELETE t100 FROM TABLE tab.

    SELECT SINGLE * FROM t100 INTO row.
    WRITE / sy-subrc.`;
    const js = await runFiles(abap, [
      {filename: "zfoobar.prog.abap", contents: code},
      {filename: "t100.tabl.xml", contents: tabl_t100xml}]);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    const cons = abap.console.get();
    expect(cons).to.equal("4");
  });

  it("SELECT SINGLE, WHERE char constant", async () => {
    const code = `
    DATA ls_result TYPE t100.
    SELECT SINGLE * FROM t100 INTO ls_result WHERE arbgb = 'ZAG_UNIT_TEST'.
    WRITE sy-subrc.`;
    const js = await runFiles(abap, [
      {filename: "zfoobar.prog.abap", contents: code},
      {filename: "t100.tabl.xml", contents: tabl_t100xml},
      {filename: "zag_unit_test.msag.xml", contents: msag_zag_unit_test}]);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("0");
  });

  it("SELECT SINGLE, WHERE AND", async () => {
    const code = `
    DATA ls_result TYPE t100.
    SELECT SINGLE * FROM t100 INTO ls_result WHERE arbgb = 'ZAG_UNIT_TEST' AND msgnr = 123.
    WRITE sy-subrc.`;
    const js = await runFiles(abap, [
      {filename: "zfoobar.prog.abap", contents: code},
      {filename: "t100.tabl.xml", contents: tabl_t100xml},
      {filename: "zag_unit_test.msag.xml", contents: msag_zag_unit_test}]);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("0");
  });

  it("SELECT SINGLE, WHERE integer constant", async () => {
    const code = `
    DATA ls_result TYPE t100.
    SELECT SINGLE * FROM t100 INTO ls_result WHERE msgnr = 123.
    WRITE sy-subrc.`;
    const js = await runFiles(abap, [
      {filename: "zfoobar.prog.abap", contents: code},
      {filename: "t100.tabl.xml", contents: tabl_t100xml},
      {filename: "zag_unit_test.msag.xml", contents: msag_zag_unit_test}]);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("0");
  });

  it("SELECT SINGLE, WHERE constant, not found", async () => {
    const code = `
    DATA ls_result TYPE t100.
    SELECT SINGLE * FROM t100 INTO ls_result WHERE arbgb = 'dsffdsfds'.
    WRITE sy-subrc.`;
    const js = await runFiles(abap, [
      {filename: "zfoobar.prog.abap", contents: code},
      {filename: "t100.tabl.xml", contents: tabl_t100xml},
      {filename: "zag_unit_test.msag.xml", contents: msag_zag_unit_test}]);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("4");
  });

  it("SELECT SINGLE, WHERE char variable", async () => {
    const code = `
    DATA ls_result TYPE t100.
    DATA lv_arbgb TYPE t100-arbgb.
    lv_arbgb = 'ZAG_UNIT_TEST'.
    SELECT SINGLE * FROM t100 INTO ls_result WHERE arbgb = lv_arbgb.
    WRITE sy-subrc.`;
    const js = await runFiles(abap, [
      {filename: "zfoobar.prog.abap", contents: code},
      {filename: "t100.tabl.xml", contents: tabl_t100xml},
      {filename: "zag_unit_test.msag.xml", contents: msag_zag_unit_test}]);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("0");
  });

  it("SELECT INTO TABLE, ORDER BY PRIMARY KEY", async () => {
    const code = `
    DATA tab TYPE STANDARD TABLE OF t100 WITH DEFAULT KEY.
    SELECT * FROM t100 INTO TABLE tab ORDER BY PRIMARY KEY.
    WRITE sy-dbcnt.`;
    const js = await runFiles(abap, [
      {filename: "zfoobar.prog.abap", contents: code},
      {filename: "t100.tabl.xml", contents: tabl_t100xml},
      {filename: "zag_unit_test.msag.xml", contents: msag_zag_unit_test}]);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("2");
  });

  it("SELECT INTO TABLE, ORDER BY PRIMARY KEY, dynamic", async () => {
    const code = `
    DATA tab TYPE STANDARD TABLE OF t100 WITH DEFAULT KEY.
    SELECT * FROM ('T100') INTO TABLE tab ORDER BY PRIMARY KEY.
    WRITE sy-dbcnt.`;
    const js = await runFiles(abap, [
      {filename: "zfoobar.prog.abap", contents: code},
      {filename: "t100.tabl.xml", contents: tabl_t100xml},
      {filename: "zag_unit_test.msag.xml", contents: msag_zag_unit_test}]);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("2");
  });

  it("basic SELECT loop", async () => {
    const code = `
    DATA bar TYPE t100.
    SELECT * FROM t100 INTO bar.
      WRITE / bar-text.
    ENDSELECT.`;
    const js = await runFiles(abap, [
      {filename: "zfoobar.prog.abap", contents: code},
      {filename: "t100.tabl.xml", contents: tabl_t100xml},
      {filename: "zag_unit_test.msag.xml", contents: msag_zag_unit_test}]);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("hello world\nblah");
  });

  it("SELECT loop, field list", async () => {
    const code = `
    DATA lv_msgnr TYPE t100-msgnr.
    DATA lv_text TYPE t100-text.
    SELECT msgnr text FROM t100 INTO (lv_msgnr, lv_text).
      WRITE / lv_text.
    ENDSELECT.`;
    const js = await runFiles(abap, [
      {filename: "zfoobar.prog.abap", contents: code},
      {filename: "t100.tabl.xml", contents: tabl_t100xml},
      {filename: "zag_unit_test.msag.xml", contents: msag_zag_unit_test}]);
    const f = new AsyncFunction("abap", js);
    await f(abap);
// TODO, for now it only checks that it compiles to valid JS
    // expect(abap.console.get()).to.equal("hello world\nblah");
  });

  it("SELECT loop, field list", async () => {
    const code = `
DATA: BEGIN OF stru,
        msgnr TYPE t100-msgnr,
        text  TYPE t100-text,
      END OF stru.
SELECT msgnr text FROM t100 INTO (stru-msgnr, stru-text).
  WRITE / stru-text.
ENDSELECT.`;
    const js = await runFiles(abap, [
      {filename: "zfoobar.prog.abap", contents: code},
      {filename: "t100.tabl.xml", contents: tabl_t100xml},
      {filename: "zag_unit_test.msag.xml", contents: msag_zag_unit_test}]);
    const f = new AsyncFunction("abap", js);
    await f(abap);
// TODO, for now it only checks that it compiles to valid JS
    // expect(abap.console.get()).to.equal("hello world\nblah");
  });

  it("SELECT COUNT(*)", async () => {
    const code = `
    SELECT COUNT(*) FROM t100.
    WRITE sy-dbcnt.`;
    const js = await runFiles(abap, [
      {filename: "zfoobar.prog.abap", contents: code},
      {filename: "t100.tabl.xml", contents: tabl_t100xml},
      {filename: "zag_unit_test.msag.xml", contents: msag_zag_unit_test}]);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("2");
  });

  it("SELECT APPENDING TABLE", async () => {
    const code = `
    DATA tab TYPE STANDARD TABLE OF t100 WITH DEFAULT KEY.
    SELECT * FROM t100 APPENDING TABLE tab ORDER BY PRIMARY KEY.
    SELECT * FROM t100 APPENDING TABLE tab ORDER BY PRIMARY KEY.
    WRITE lines( tab ).`;
    const js = await runFiles(abap, [
      {filename: "zfoobar.prog.abap", contents: code},
      {filename: "t100.tabl.xml", contents: tabl_t100xml},
      {filename: "zag_unit_test.msag.xml", contents: msag_zag_unit_test}]);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("4");
  });

  it("FOR ALL ENTRIES, basic, single row", async () => {
    const code = `
    DATA lt_t100 TYPE STANDARD TABLE OF t100 WITH DEFAULT KEY.
    DATA lt_fae TYPE STANDARD TABLE OF t100 WITH DEFAULT KEY.
    DATA ls_fae LIKE LINE OF lt_fae.

    ls_fae-msgnr = '123'.
    INSERT ls_fae INTO TABLE lt_fae.

    SELECT * FROM t100 INTO TABLE lt_t100
      FOR ALL ENTRIES IN lt_fae
      WHERE msgnr = lt_fae-msgnr.
    WRITE sy-dbcnt.`;
    const js = await runFiles(abap, [
      {filename: "zfoobar.prog.abap", contents: code},
      {filename: "t100.tabl.xml", contents: tabl_t100xml},
      {filename: "zag_unit_test.msag.xml", contents: msag_zag_unit_test}]);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("1");
  });

  it("FOR ALL ENTRIES, condition not true", async () => {
    const code = `
DATA lt_t100 TYPE STANDARD TABLE OF t100 WITH DEFAULT KEY.
DATA lt_fae TYPE STANDARD TABLE OF t100 WITH DEFAULT KEY.
DATA ls_fae LIKE LINE OF lt_fae.

ls_fae-msgnr = '123'.
ls_fae-arbgb = '123'.
INSERT ls_fae INTO TABLE lt_fae.

SELECT * FROM t100 INTO TABLE lt_t100
  FOR ALL ENTRIES IN lt_fae
  WHERE msgnr = lt_fae-msgnr
  AND arbgb = lt_fae-arbgb.
WRITE sy-dbcnt.
WRITE lines( lt_t100 ).`;
    const js = await runFiles(abap, [
      {filename: "zfoobar.prog.abap", contents: code},
      {filename: "t100.tabl.xml", contents: tabl_t100xml},
      {filename: "zag_unit_test.msag.xml", contents: msag_zag_unit_test}]);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("00");
  });

  it("FOR ALL ENTRIES, table line", async () => {
    const code = `
DATA lt_t100 TYPE STANDARD TABLE OF t100 WITH DEFAULT KEY.
DATA lt_msgnr TYPE STANDARD TABLE OF t100-msgnr WITH DEFAULT KEY.
INSERT '123' INTO TABLE lt_msgnr.
SELECT * FROM t100 INTO TABLE lt_t100
  FOR ALL ENTRIES IN lt_msgnr
  WHERE msgnr = lt_msgnr-table_line.
WRITE sy-dbcnt.`;
    const js = await runFiles(abap, [
      {filename: "zfoobar.prog.abap", contents: code},
      {filename: "t100.tabl.xml", contents: tabl_t100xml},
      {filename: "zag_unit_test.msag.xml", contents: msag_zag_unit_test}]);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("1");
  });

  it("Test escaping single ping", async () => {
    const code = `
    DATA ls_result TYPE t100.
    SELECT SINGLE * FROM t100 INTO ls_result.
    WRITE ls_result-text.`;
    const js = await runFiles(abap, [
      {filename: "zfoobar.prog.abap", contents: code},
      {filename: "t100.tabl.xml", contents: tabl_t100xml},
      {filename: "zescape.msag.xml", contents: msag_escape}]);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("FOO 'HELLO' bar");
  });

  it("SELECT list of columns", async () => {
    const code = `
    DATA ls_result TYPE t100.
    SELECT SINGLE arbgb msgnr text FROM t100 INTO CORRESPONDING FIELDS OF ls_result.
    WRITE ls_result-arbgb.`;
    const js = await runFiles(abap, [
      {filename: "zfoobar.prog.abap", contents: code},
      {filename: "t100.tabl.xml", contents: tabl_t100xml},
      {filename: "zag_unit_test.msag.xml", contents: msag_zag_unit_test}]);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("ZAG_UNIT_TEST");
  });

  it("SELECT WHERE value from structure", async () => {
    const code = `
DATA: BEGIN OF foo,
        arbgb TYPE t100-arbgb,
      END OF foo.
foo-arbgb = 'ZAG_UNIT_TEST'.
DATA ls_result TYPE t100.
SELECT SINGLE * FROM t100 INTO ls_result WHERE arbgb = foo-arbgb.
ASSERT sy-subrc = 0.`;
    const js = await runFiles(abap, [
      {filename: "zfoobar.prog.abap", contents: code},
      {filename: "t100.tabl.xml", contents: tabl_t100xml},
      {filename: "zag_unit_test.msag.xml", contents: msag_zag_unit_test}]);
    const f = new AsyncFunction("abap", js);
    await f(abap);
  });

  it("INTO TABLE UP TO ORDER BY", async () => {
    const code = `
    DATA lt_t100 TYPE STANDARD TABLE OF t100.
    SELECT * FROM t100 INTO TABLE lt_t100 UP TO 5 ROWS ORDER BY PRIMARY KEY.
    WRITE sy-dbcnt.`;
    const js = await runFiles(abap, [
      {filename: "zfoobar.prog.abap", contents: code},
      {filename: "t100.tabl.xml", contents: tabl_t100xml},
      {filename: "zag_unit_test.msag.xml", contents: msag_zag_unit_test}]);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("2");
  });

  it("INSERT FROM", async () => {
    const code = `
    DATA ls_t100 TYPE t100.

    SELECT SINGLE * FROM t100 INTO ls_t100.
    ASSERT sy-subrc = 4.

    ls_t100-arbgb = 'HELLO'.
    INSERT t100 FROM ls_t100.
    ASSERT sy-subrc = 0.

    SELECT SINGLE * FROM t100 INTO ls_t100 WHERE arbgb = 'HELLO'.
    ASSERT sy-subrc = 0.`;
    const js = await runFiles(abap, [
      {filename: "zfoobar.prog.abap", contents: code},
      {filename: "t100.tabl.xml", contents: tabl_t100xml}]);
    const f = new AsyncFunction("abap", js);
    await f(abap);
  });

  it("INSERT FROM, escape ampersand", async () => {
    const code = `
    DATA ls_t100 TYPE t100.
    ls_t100-arbgb = '"'.
    INSERT t100 FROM ls_t100.
    ASSERT sy-subrc = 0.

    CLEAR ls_t100.
    SELECT SINGLE * FROM t100 INTO ls_t100.
    ASSERT sy-subrc = 0.
    ASSERT ls_t100-arbgb = '"'.`;
    const js = await runFiles(abap, [
      {filename: "zfoobar.prog.abap", contents: code},
      {filename: "t100.tabl.xml", contents: tabl_t100xml}]);
    const f = new AsyncFunction("abap", js);
    await f(abap);
  });

  it.only("SELECT, IN", async () => {
    const code = `
    TYPES ty_range TYPE RANGE OF t100-arbgb.
    DATA lt_t100 TYPE STANDARD TABLE OF t100 WITH DEFAULT KEY.
    DATA lt_range TYPE ty_range.
    DATA ls_range LIKE LINE OF lt_range.

    SELECT * FROM t100 INTO TABLE lt_t100 WHERE arbgb IN lt_range.
    WRITE / sy-dbcnt.

    ls_range-low = 'ZAG_UNIT_TEST'.
    ls_range-sign = 'I'.
    ls_range-option = 'EQ'.
    APPEND ls_range TO lt_range.

    SELECT * FROM t100 INTO TABLE lt_t100 WHERE arbgb IN lt_range.
    WRITE / sy-dbcnt.`;
    const js = await runFiles(abap, [
      {filename: "zfoobar.prog.abap", contents: code},
      {filename: "t100.tabl.xml", contents: tabl_t100xml},
      {filename: "zag_unit_test.msag.xml", contents: msag_zag_unit_test}]);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("2\n2");
  });

});