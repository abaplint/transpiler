import {expect} from "chai";
import {AsyncFunction, runFiles as runRilesSqlite, runFilesPostgres} from "./_utils";
import {ABAP, MemoryConsole} from "../packages/runtime/src/";
import {msag_escape, msag_zag_unit_test, tabl_t100xml, zt111, zt222} from "./_data";
import {IFile} from "../packages/transpiler/src/types";

async function runAllDatabases(abap: ABAP,
                               files: IFile[],
                               check: () => any,
                               settings = {sqlite: true, postgres: true}) {

  if (settings.sqlite === true) {
    const js = await runRilesSqlite(abap, files);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    check();
  }

  if (settings.postgres === true) {
    const js = await runFilesPostgres(abap, files);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    check();
  }
}

/////////////////////////////////////////////////////

describe("Top level tests, Database", () => {
  let abap: ABAP;

  beforeEach(async () => {
    abap = new ABAP({console: new MemoryConsole()});
  });

  it("SELECT", async () => {
    const code = `
    DATA ls_result TYPE t100.
    SELECT SINGLE * FROM t100 INTO ls_result.
    WRITE ls_result-text.`;
    const files = [
      {filename: "zfoobar.prog.abap", contents: code},
      {filename: "t100.tabl.xml", contents: tabl_t100xml},
      {filename: "zag_unit_test.msag.xml", contents: msag_zag_unit_test},
    ];
    await runAllDatabases(abap, files, () => {
      expect(abap.console.get().trimEnd()).to.equal("hello world");
    });
  });

  it("SELECT, no result", async () => {
    const code = `
    DATA ls_result TYPE t100.
    SELECT SINGLE * FROM t100 INTO ls_result.
    WRITE sy-subrc.`;
    const files = [
      {filename: "zfoobar.prog.abap", contents: code},
      {filename: "t100.tabl.xml", contents: tabl_t100xml}];
    await runAllDatabases(abap, files, () => {
      expect(abap.console.get()).to.equal("4");
    });
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
    const files = [
      {filename: "zfoobar.prog.abap", contents: code},
      {filename: "t100.tabl.xml", contents: tabl_t100xml}];
    await runAllDatabases(abap, files, () => {
      expect(abap.console.get().trimEnd()).to.equal("0\nHELLO");
    });
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
    const files = [
      {filename: "zfoobar.prog.abap", contents: code},
      {filename: "t100.tabl.xml", contents: tabl_t100xml}];
    await runAllDatabases(abap, files, () => {
      expect(abap.console.get().trimEnd()).to.equal("WORLD");
    });
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
    const files = [
      {filename: "zfoobar.prog.abap", contents: code},
      {filename: "t100.tabl.xml", contents: tabl_t100xml}];
    await runAllDatabases(abap, files, () => {
      const cons = abap.console.get();
      expect(cons).to.equal("4");
    });
  });

  it("SELECT SINGLE, WHERE char constant", async () => {
    const code = `
    DATA ls_result TYPE t100.
    SELECT SINGLE * FROM t100 INTO ls_result WHERE arbgb = 'ZAG_UNIT_TEST'.
    WRITE sy-subrc.`;
    const files = [
      {filename: "zfoobar.prog.abap", contents: code},
      {filename: "t100.tabl.xml", contents: tabl_t100xml},
      {filename: "zag_unit_test.msag.xml", contents: msag_zag_unit_test}];
    await runAllDatabases(abap, files, () => {
      expect(abap.console.get()).to.equal("0");
    });
  });

  it("SELECT SINGLE, WHERE AND", async () => {
    const code = `
    DATA ls_result TYPE t100.
    SELECT SINGLE * FROM t100 INTO ls_result WHERE arbgb = 'ZAG_UNIT_TEST' AND msgnr = 123.
    WRITE sy-subrc.`;
    const files = [
      {filename: "zfoobar.prog.abap", contents: code},
      {filename: "t100.tabl.xml", contents: tabl_t100xml},
      {filename: "zag_unit_test.msag.xml", contents: msag_zag_unit_test}];
    await runAllDatabases(abap, files, () => {
      expect(abap.console.get()).to.equal("0");
    });
  });

  it("SELECT SINGLE, WHERE integer constant", async () => {
    const code = `
    DATA ls_result TYPE t100.
    SELECT SINGLE * FROM t100 INTO ls_result WHERE msgnr = 123.
    WRITE sy-subrc.`;
    const files = [
      {filename: "zfoobar.prog.abap", contents: code},
      {filename: "t100.tabl.xml", contents: tabl_t100xml},
      {filename: "zag_unit_test.msag.xml", contents: msag_zag_unit_test}];
    await runAllDatabases(abap, files, () => {
      expect(abap.console.get()).to.equal("0");
    });
  });

  it("SELECT SINGLE, WHERE constant, not found", async () => {
    const code = `
    DATA ls_result TYPE t100.
    SELECT SINGLE * FROM t100 INTO ls_result WHERE arbgb = 'dsffdsfds'.
    WRITE sy-subrc.`;
    const files = [
      {filename: "zfoobar.prog.abap", contents: code},
      {filename: "t100.tabl.xml", contents: tabl_t100xml},
      {filename: "zag_unit_test.msag.xml", contents: msag_zag_unit_test}];
    await runAllDatabases(abap, files, () => {
      expect(abap.console.get()).to.equal("4");
    });
  });

  it("SELECT SINGLE, WHERE char variable", async () => {
    const code = `
    DATA ls_result TYPE t100.
    DATA lv_arbgb TYPE t100-arbgb.
    lv_arbgb = 'ZAG_UNIT_TEST'.
    SELECT SINGLE * FROM t100 INTO ls_result WHERE arbgb = lv_arbgb.
    WRITE sy-subrc.`;
    const files = [
      {filename: "zfoobar.prog.abap", contents: code},
      {filename: "t100.tabl.xml", contents: tabl_t100xml},
      {filename: "zag_unit_test.msag.xml", contents: msag_zag_unit_test}];
    await runAllDatabases(abap, files, () => {
      expect(abap.console.get()).to.equal("0");
    });
  });

  it("SELECT INTO TABLE, ORDER BY PRIMARY KEY", async () => {
    const code = `
    DATA tab TYPE STANDARD TABLE OF t100 WITH DEFAULT KEY.
    SELECT * FROM t100 INTO TABLE tab ORDER BY PRIMARY KEY.
    WRITE sy-dbcnt.`;
    const files = [
      {filename: "zfoobar.prog.abap", contents: code},
      {filename: "t100.tabl.xml", contents: tabl_t100xml},
      {filename: "zag_unit_test.msag.xml", contents: msag_zag_unit_test}];
    await runAllDatabases(abap, files, () => {
      expect(abap.console.get()).to.equal("2");
    });
  });

  it("SELECT INTO TABLE, ORDER BY PRIMARY KEY, dynamic", async () => {
    const code = `
    DATA tab TYPE STANDARD TABLE OF t100 WITH DEFAULT KEY.
    SELECT * FROM ('T100') INTO TABLE tab ORDER BY PRIMARY KEY.
    WRITE sy-dbcnt.`;
    const files = [
      {filename: "zfoobar.prog.abap", contents: code},
      {filename: "t100.tabl.xml", contents: tabl_t100xml},
      {filename: "zag_unit_test.msag.xml", contents: msag_zag_unit_test}];
    await runAllDatabases(abap, files, () => {
      expect(abap.console.get()).to.equal("2");
    });
  });

  it("basic SELECT loop", async () => {
    const code = `
    DATA bar TYPE t100.
    SELECT * FROM t100 INTO bar.
      WRITE / bar-text.
    ENDSELECT.`;
    const files = [
      {filename: "zfoobar.prog.abap", contents: code},
      {filename: "t100.tabl.xml", contents: tabl_t100xml},
      {filename: "zag_unit_test.msag.xml", contents: msag_zag_unit_test}];
    await runAllDatabases(abap, files, () => {
      expect(abap.console.getTrimmed()).to.equal("hello world\nblah");
    });
  });

  it("SELECT loop, field list", async () => {
    const code = `
    DATA lv_msgnr TYPE t100-msgnr.
    DATA lv_text TYPE t100-text.
    SELECT msgnr text FROM t100 INTO (lv_msgnr, lv_text).
      WRITE / lv_text.
    ENDSELECT.`;
    const files = [
      {filename: "zfoobar.prog.abap", contents: code},
      {filename: "t100.tabl.xml", contents: tabl_t100xml},
      {filename: "zag_unit_test.msag.xml", contents: msag_zag_unit_test}];
    await runAllDatabases(abap, files, () => {
// TODO, for now it only checks that it compiles to valid JS
    // expect(abap.console.get()).to.equal("hello world\nblah");
    });
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
    const files = [
      {filename: "zfoobar.prog.abap", contents: code},
      {filename: "t100.tabl.xml", contents: tabl_t100xml},
      {filename: "zag_unit_test.msag.xml", contents: msag_zag_unit_test}];
    await runAllDatabases(abap, files, () => {
// TODO, for now it only checks that it compiles to valid JS
    // expect(abap.console.get()).to.equal("hello world\nblah");
    });
  });

  it("SELECT COUNT(*)", async () => {
    const code = `
    SELECT COUNT(*) FROM t100.
    WRITE sy-dbcnt.`;
    const files = [
      {filename: "zfoobar.prog.abap", contents: code},
      {filename: "t100.tabl.xml", contents: tabl_t100xml},
      {filename: "zag_unit_test.msag.xml", contents: msag_zag_unit_test}];
    await runAllDatabases(abap, files, () => {
      expect(abap.console.get()).to.equal("2");
    });
  });

  it("SELECT APPENDING TABLE", async () => {
    const code = `
    DATA tab TYPE STANDARD TABLE OF t100 WITH DEFAULT KEY.
    SELECT * FROM t100 APPENDING TABLE tab ORDER BY PRIMARY KEY.
    SELECT * FROM t100 APPENDING TABLE tab ORDER BY PRIMARY KEY.
    WRITE lines( tab ).`;
    const files = [
      {filename: "zfoobar.prog.abap", contents: code},
      {filename: "t100.tabl.xml", contents: tabl_t100xml},
      {filename: "zag_unit_test.msag.xml", contents: msag_zag_unit_test}];
    await runAllDatabases(abap, files, () => {
      expect(abap.console.get()).to.equal("4");
    });
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
    const files = [
      {filename: "zfoobar.prog.abap", contents: code},
      {filename: "t100.tabl.xml", contents: tabl_t100xml},
      {filename: "zag_unit_test.msag.xml", contents: msag_zag_unit_test}];
    await runAllDatabases(abap, files, () => {
      expect(abap.console.get()).to.equal("1");
    });
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
    const files = [
      {filename: "zfoobar.prog.abap", contents: code},
      {filename: "t100.tabl.xml", contents: tabl_t100xml},
      {filename: "zag_unit_test.msag.xml", contents: msag_zag_unit_test}];
    await runAllDatabases(abap, files, () => {
      expect(abap.console.get()).to.equal("00");
    });
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
    const files = [
      {filename: "zfoobar.prog.abap", contents: code},
      {filename: "t100.tabl.xml", contents: tabl_t100xml},
      {filename: "zag_unit_test.msag.xml", contents: msag_zag_unit_test}];
    await runAllDatabases(abap, files, () => {
      expect(abap.console.get()).to.equal("1");
    });
  });

  it("FOR ALL ENTRIES, table line, hmm", async () => {
    const code = `
DATA lt_t100 TYPE STANDARD TABLE OF t100 WITH DEFAULT KEY.
DATA lt_msgnr TYPE STANDARD TABLE OF t100-msgnr WITH DEFAULT KEY.
INSERT '123' INTO TABLE lt_msgnr.
select * from t100
         into table lt_t100
         for all entries in lt_msgnr
         where msgnr = lt_msgnr-table_line.
WRITE sy-dbcnt.`;
    const files = [
      {filename: "zfoobar.prog.abap", contents: code},
      {filename: "t100.tabl.xml", contents: tabl_t100xml},
      {filename: "zag_unit_test.msag.xml", contents: msag_zag_unit_test}];
    await runAllDatabases(abap, files, () => {
      expect(abap.console.get()).to.equal("1");
    });
  });

  it("Test escaping single ping", async () => {
    const code = `
    DATA ls_result TYPE t100.
    SELECT SINGLE * FROM t100 INTO ls_result.
    WRITE ls_result-text.`;
    const files = [
      {filename: "zfoobar.prog.abap", contents: code},
      {filename: "t100.tabl.xml", contents: tabl_t100xml},
      {filename: "zescape.msag.xml", contents: msag_escape}];
    await runAllDatabases(abap, files, () => {
      expect(abap.console.get().trimEnd()).to.equal("FOO 'HELLO' bar");
    });
  });

  it("SELECT list of columns", async () => {
    const code = `
    DATA ls_result TYPE t100.
    SELECT SINGLE arbgb msgnr text FROM t100 INTO CORRESPONDING FIELDS OF ls_result.
    WRITE ls_result-arbgb.`;
    const files = [
      {filename: "zfoobar.prog.abap", contents: code},
      {filename: "t100.tabl.xml", contents: tabl_t100xml},
      {filename: "zag_unit_test.msag.xml", contents: msag_zag_unit_test}];
    await runAllDatabases(abap, files, () => {
      expect(abap.console.get().trimEnd()).to.equal("ZAG_UNIT_TEST");
    });
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
    const files = [
      {filename: "zfoobar.prog.abap", contents: code},
      {filename: "t100.tabl.xml", contents: tabl_t100xml},
      {filename: "zag_unit_test.msag.xml", contents: msag_zag_unit_test}];
    await runAllDatabases(abap, files, () => {
      // just check its valid js
    });
  });

  it("INTO TABLE UP TO ORDER BY", async () => {
    const code = `
    DATA lt_t100 TYPE STANDARD TABLE OF t100.
    SELECT * FROM t100 INTO TABLE lt_t100 UP TO 5 ROWS ORDER BY PRIMARY KEY.
    WRITE sy-dbcnt.`;
    const files = [
      {filename: "zfoobar.prog.abap", contents: code},
      {filename: "t100.tabl.xml", contents: tabl_t100xml},
      {filename: "zag_unit_test.msag.xml", contents: msag_zag_unit_test}];
    await runAllDatabases(abap, files, () => {
      expect(abap.console.get()).to.equal("2");
    });
  });

  it("INTO TABLE UP TO ORDER BY, var", async () => {
    const code = `
    DATA lt_t100 TYPE STANDARD TABLE OF t100.
    DATA lv_count TYPE i.
    lv_count = 5.
    SELECT * FROM t100 INTO TABLE lt_t100 UP TO lv_count ROWS ORDER BY PRIMARY KEY.
    WRITE sy-dbcnt.`;
    const files = [
      {filename: "zfoobar.prog.abap", contents: code},
      {filename: "t100.tabl.xml", contents: tabl_t100xml},
      {filename: "zag_unit_test.msag.xml", contents: msag_zag_unit_test}];
    await runAllDatabases(abap, files, () => {
      expect(abap.console.get()).to.equal("2");
    });
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
    const files = [
      {filename: "zfoobar.prog.abap", contents: code},
      {filename: "t100.tabl.xml", contents: tabl_t100xml}];
    await runAllDatabases(abap, files, () => {
      // just check valid js
    });
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
    const files = [
      {filename: "zfoobar.prog.abap", contents: code},
      {filename: "t100.tabl.xml", contents: tabl_t100xml}];
    await runAllDatabases(abap, files, () => {
      // just check valid js
    });
  });

  it("SELECT, IN", async () => {
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
    const files = [
      {filename: "zfoobar.prog.abap", contents: code},
      {filename: "t100.tabl.xml", contents: tabl_t100xml},
      {filename: "zag_unit_test.msag.xml", contents: msag_zag_unit_test}];
    await runAllDatabases(abap, files, () => {
      expect(abap.console.get()).to.equal("2\n2");
    });
  });

  it("SELECT, dynamic WHERE condition, constants", async () => {
    const code = `
    DATA lt_t100 TYPE STANDARD TABLE OF t100 WITH DEFAULT KEY.
    DATA lv_where TYPE string.
    lv_where = |ARBGB = 'ZAG_UNIT_TEST'|.
    SELECT * FROM t100
      INTO TABLE lt_t100
      WHERE (lv_where)
      ORDER BY PRIMARY KEY.
    WRITE / sy-dbcnt.`;
    const files = [
      {filename: "zfoobar.prog.abap", contents: code},
      {filename: "t100.tabl.xml", contents: tabl_t100xml},
      {filename: "zag_unit_test.msag.xml", contents: msag_zag_unit_test}];
    await runAllDatabases(abap, files, () => {
      expect(abap.console.get()).to.equal("2");
    });
  });

  it("DELETE WHERE", async () => {
    const code = `
    DATA ls_t100 TYPE t100.

    SELECT SINGLE * FROM t100 INTO ls_t100.
    ASSERT sy-subrc = 4.

    ls_t100-sprsl = 'E'.
    ls_t100-arbgb = 'HELLO'.
    INSERT t100 FROM ls_t100.
    ASSERT sy-subrc = 0.

    ls_t100-sprsl = 'K'.
    ls_t100-arbgb = 'HELLO'.
    INSERT t100 FROM ls_t100.
    ASSERT sy-subrc = 0.

    DELETE FROM t100 WHERE arbgb = 'HELLO'.
    ASSERT sy-subrc = 0.
    ASSERT sy-dbcnt = 2.

    DELETE FROM t100 WHERE arbgb = 'HELLO'.
    ASSERT sy-subrc = 4.
    ASSERT sy-dbcnt = 0.`;
    const files = [
      {filename: "zfoobar.prog.abap", contents: code},
      {filename: "t100.tabl.xml", contents: tabl_t100xml}];
    await runAllDatabases(abap, files, () => {
      // just check its valid js
    });
  });

  it("LIKE ESCAPE", async () => {
    const code = `
    DATA ls_t100 TYPE t100.
    SELECT SINGLE * FROM t100 INTO ls_t100 WHERE arbgb LIKE 'Z%' ESCAPE '#'.
    WRITE sy-dbcnt.`;
    const files = [
      {filename: "zfoobar.prog.abap", contents: code},
      {filename: "t100.tabl.xml", contents: tabl_t100xml},
      {filename: "zag_unit_test.msag.xml", contents: msag_zag_unit_test}];
    await runAllDatabases(abap, files, () => {
      expect(abap.console.get()).to.equal("1");
    });
  });

  it("INTO simple", async () => {
    const code = `
    DATA lv_count TYPE i.
    SELECT COUNT(*) FROM t100 INTO lv_count.
    WRITE lv_count.`;
    const files = [
      {filename: "zfoobar.prog.abap", contents: code},
      {filename: "t100.tabl.xml", contents: tabl_t100xml},
      {filename: "zag_unit_test.msag.xml", contents: msag_zag_unit_test}];
    await runAllDatabases(abap, files, () => {
      expect(abap.console.get()).to.equal("2");
    });
  });

  it("MODIFY simple", async () => {
    const code = `
    DATA ls_t100 TYPE t100.
    ls_t100-text = 'foo'.
    MODIFY t100 FROM ls_t100.
    WRITE sy-subrc.
    ls_t100-text = '"'.
    MODIFY t100 FROM ls_t100.
    WRITE sy-subrc.`;
    const files = [
      {filename: "zfoobar.prog.abap", contents: code},
      {filename: "t100.tabl.xml", contents: tabl_t100xml},
      {filename: "zag_unit_test.msag.xml", contents: msag_zag_unit_test}];
    await runAllDatabases(abap, files, () => {
      expect(abap.console.get()).to.equal("00");
    });
  });

  it("INSERT FROM TABLE", async () => {
    const code = `
    DATA tab TYPE STANDARD TABLE OF t100 WITH DEFAULT KEY.
    DATA lv_count TYPE i.

    APPEND INITIAL LINE TO tab.
    INSERT t100 FROM TABLE tab.

    SELECT COUNT(*) FROM t100 INTO lv_count.
    WRITE lv_count.`;
    const files = [
      {filename: "zfoobar.prog.abap", contents: code},
      {filename: "t100.tabl.xml", contents: tabl_t100xml},
      {filename: "zag_unit_test.msag.xml", contents: msag_zag_unit_test}];
    await runAllDatabases(abap, files, () => {
      expect(abap.console.get()).to.equal("3");
    });
  });

  it("tilde", async () => {
    const code = `
    DATA lv_msgnr TYPE t100-msgnr.
    SELECT SINGLE t100~msgnr FROM t100 INTO lv_msgnr.
    WRITE sy-dbcnt.`;
    const files = [
      {filename: "zfoobar.prog.abap", contents: code},
      {filename: "t100.tabl.xml", contents: tabl_t100xml},
      {filename: "zag_unit_test.msag.xml", contents: msag_zag_unit_test}];
    await runAllDatabases(abap, files, () => {
      expect(abap.console.get()).to.equal("1");
    });
  });

  it("inner join", async () => {
    const code = `
    DATA lv_msgnr TYPE t100-msgnr.
    SELECT SINGLE t100~msgnr FROM t100
      INNER JOIN t100 AS foo
      ON t100~msgnr = foo~msgnr
      INTO lv_msgnr.
    WRITE sy-dbcnt.`;
    const files = [
      {filename: "zfoobar.prog.abap", contents: code},
      {filename: "t100.tabl.xml", contents: tabl_t100xml},
      {filename: "zag_unit_test.msag.xml", contents: msag_zag_unit_test}];
    await runAllDatabases(abap, files, () => {
      expect(abap.console.get()).to.equal("1");
    });
  });

  it("inner join with variable", async () => {
    const code = `
    DATA lv_msgnr TYPE t100-msgnr.
    SELECT SINGLE t100~msgnr INTO lv_msgnr FROM t100
      INNER JOIN t100 AS foo
      ON t100~msgnr = foo~msgnr
      AND t100~msgnr = lv_msgnr.`;
    const files = [
      {filename: "zfoobar.prog.abap", contents: code},
      {filename: "t100.tabl.xml", contents: tabl_t100xml},
      {filename: "zag_unit_test.msag.xml", contents: msag_zag_unit_test}];
    await runAllDatabases(abap, files, () => {
      // just check it compiles and runs
    });
  });

  it(".INCLUDE with GROUPNAME", async () => {
    const code = `
    DATA foo TYPE zt111.
    SELECT SINGLE * FROM zt111 INTO foo.`;
    const files = [
      {filename: "zfoobar.prog.abap", contents: code},
      {filename: "zt111.tabl.xml", contents: zt111},
      {filename: "zt222.tabl.xml", contents: zt222}];
    await runAllDatabases(abap, files, () => {
      // just check it compiles and runs
    });
  });

  it("SELECT LIKE", async () => {
    const code = `
    DATA ls_result TYPE t100.
    SELECT SINGLE * FROM t100 INTO ls_result WHERE text LIKE 'h%'.
    WRITE ls_result-text.`;
    const files = [
      {filename: "zfoobar.prog.abap", contents: code},
      {filename: "t100.tabl.xml", contents: tabl_t100xml},
      {filename: "zag_unit_test.msag.xml", contents: msag_zag_unit_test}];
    await runAllDatabases(abap, files, () => {
      expect(abap.console.get().trimEnd()).to.equal("hello world");
    });
  });

  it("FAE with field symbol", async () => {
    const code = `
TYPES: BEGIN OF ty,
         msgnr TYPE t100-msgnr,
       END OF ty.

DATA tab TYPE STANDARD TABLE OF ty WITH DEFAULT KEY.
DATA result TYPE STANDARD TABLE OF t100 WITH DEFAULT KEY.
FIELD-SYMBOLS <foo> LIKE tab.
ASSIGN tab TO <foo>.
APPEND INITIAL LINE TO tab.

SELECT * FROM t100 INTO TABLE result
  FOR ALL ENTRIES IN <foo>
  WHERE msgnr = <foo>-msgnr.`;
    const files = [
      {filename: "zfoobar.prog.abap", contents: code},
      {filename: "t100.tabl.xml", contents: tabl_t100xml}];
    await runAllDatabases(abap, files, () => {
      // just check it compiles and runs
    });
  });

  it("dynamic INTO CORRESPONDING FIELDS OF field symbol", async () => {
    const code = `
DATA row TYPE t100.
FIELD-SYMBOLS <fs> TYPE t100.
ASSIGN row TO <fs>.
SELECT * INTO CORRESPONDING FIELDS OF <fs> FROM t100.
ENDSELECT.`;
    const files = [
      {filename: "zfoobar.prog.abap", contents: code},
      {filename: "t100.tabl.xml", contents: tabl_t100xml}];
    await runAllDatabases(abap, files, () => {
      // just check it compiles and runs
    });
  });

  it("basic TABLES", async () => {
    const code = `
TABLES t100.
CLEAR t100.`;
    const files = [
      {filename: "zfoobar.prog.abap", contents: code},
      {filename: "t100.tabl.xml", contents: tabl_t100xml}];
    await runAllDatabases(abap, files, () => {
      // just check it compiles and runs
    });
  });

  it("DESCENDING", async () => {
    const code = `
DATA ls_t100 TYPE t100.
SELECT * FROM t100 INTO ls_t100 UP TO 1 ROWS ORDER BY msgnr DESCENDING.
ENDSELECT.`;
    const files = [
      {filename: "zfoobar.prog.abap", contents: code},
      {filename: "t100.tabl.xml", contents: tabl_t100xml}];
    await runAllDatabases(abap, files, () => {
      // just check it compiles and runs
    });
  });

  it("ASCENDING", async () => {
    const code = `
DATA ls_t100 TYPE t100.
SELECT * FROM t100 INTO ls_t100 UP TO 1 ROWS ORDER BY msgnr ASCENDING.
ENDSELECT.`;
    const files = [
      {filename: "zfoobar.prog.abap", contents: code},
      {filename: "t100.tabl.xml", contents: tabl_t100xml}];
    await runAllDatabases(abap, files, () => {
      // just check it compiles and runs
    });
  });

  it("UPDATE, success", async () => {
    const code = `
UPDATE t100 SET text = 'sdf' WHERE msgnr = '123'.
WRITE / sy-dbcnt.
WRITE / sy-subrc.`;
    const files = [
      {filename: "zfoobar.prog.abap", contents: code},
      {filename: "t100.tabl.xml", contents: tabl_t100xml},
      {filename: "zag_unit_test.msag.xml", contents: msag_zag_unit_test}];
    await runAllDatabases(abap, files, () => {
      expect(abap.console.get()).to.equal("1\n0");
    });
  });

  it("WHERE, empty dynamic condition", async () => {
    const code = `
DATA tab TYPE STANDARD TABLE OF t100 WITH DEFAULT KEY.
DATA lv TYPE string.
SELECT * FROM t100 INTO TABLE tab WHERE (lv).
WRITE sy-dbcnt.`;
    const files = [
      {filename: "zfoobar.prog.abap", contents: code},
      {filename: "t100.tabl.xml", contents: tabl_t100xml},
      {filename: "zag_unit_test.msag.xml", contents: msag_zag_unit_test},
    ];
    await runAllDatabases(abap, files, () => {
      expect(abap.console.get()).to.equal("2");
    });
  });

  it("SELECT SINGLE, WHERE eq", async () => {
    const code = `
    DATA ls_result TYPE t100.
    SELECT SINGLE * FROM t100 INTO ls_result WHERE arbgb eq 'ZAG_UNIT_TEST'.
    WRITE sy-subrc.`;
    const files = [
      {filename: "zfoobar.prog.abap", contents: code},
      {filename: "t100.tabl.xml", contents: tabl_t100xml},
      {filename: "zag_unit_test.msag.xml", contents: msag_zag_unit_test}];
    await runAllDatabases(abap, files, () => {
      expect(abap.console.get()).to.equal("0");
    });
  });

  it("SELECT SINGLE, WHERE EQ", async () => {
    const code = `
    DATA ls_result TYPE t100.
    SELECT SINGLE * FROM t100 INTO ls_result WHERE arbgb EQ 'ZAG_UNIT_TEST'.
    WRITE sy-subrc.`;
    const files = [
      {filename: "zfoobar.prog.abap", contents: code},
      {filename: "t100.tabl.xml", contents: tabl_t100xml},
      {filename: "zag_unit_test.msag.xml", contents: msag_zag_unit_test}];
    await runAllDatabases(abap, files, () => {
      expect(abap.console.get()).to.equal("0");
    });
  });

  it("SELECT SINGLE, constant from interface", async () => {
    const code = `
INTERFACE /foo/bar.
  CONSTANTS foo TYPE t100-arbgb VALUE 'ZAG_UNIT_TEST'.
ENDINTERFACE.

CLASS foo DEFINITION.
  PUBLIC SECTION.
    INTERFACES /foo/bar.
    CLASS-METHODS run.
ENDCLASS.

CLASS foo IMPLEMENTATION.
  METHOD run.
    DATA ls_result TYPE t100.
    SELECT SINGLE * FROM t100 INTO ls_result WHERE arbgb EQ /foo/bar~foo.
    WRITE sy-subrc.
  ENDMETHOD.
ENDCLASS.

START-OF-SELECTION.
  foo=>run( ).`;
    const files = [
      {filename: "zfoobar.prog.abap", contents: code},
      {filename: "t100.tabl.xml", contents: tabl_t100xml},
      {filename: "zag_unit_test.msag.xml", contents: msag_zag_unit_test}];
    await runAllDatabases(abap, files, () => {
      expect(abap.console.get()).to.equal("0");
    });
  });

  it("SELECT into non structured table", async () => {
    const code = `
DATA lt_data TYPE STANDARD TABLE OF t100-arbgb WITH NON-UNIQUE KEY table_line.
DATA lv_data LIKE LINE OF lt_data.

SELECT arbgb
  FROM t100
  INTO TABLE lt_data
  WHERE arbgb = 'ZAG_UNIT_TEST'.

LOOP AT lt_data INTO lv_data.
  WRITE / lv_data.
ENDLOOP.`;
    const files = [
      {filename: "zfoobar.prog.abap", contents: code},
      {filename: "t100.tabl.xml", contents: tabl_t100xml},
      {filename: "zag_unit_test.msag.xml", contents: msag_zag_unit_test}];
    await runAllDatabases(abap, files, () => {
      expect(abap.console.get()).to.equal("ZAG_UNIT_TEST       \nZAG_UNIT_TEST       ");
    });
  });

  it("SELECT dynamic field symbol", async () => {
    const code = `
DATA val TYPE t100-arbgb.
DATA ls_t100 TYPE t100.
FIELD-SYMBOLS <fs> TYPE t100-arbgb.
DATA lv_bar TYPE string.
val = 'ZAG_UNIT_TEST'.
ASSIGN val TO <fs>.
lv_bar = 'arbgb = <fs>'.
SELECT SINGLE * FROM t100 INTO ls_t100 WHERE (lv_bar).
WRITE sy-dbcnt.`;
    const files = [
      {filename: "zfoobar.prog.abap", contents: code},
      {filename: "t100.tabl.xml", contents: tabl_t100xml},
      {filename: "zag_unit_test.msag.xml", contents: msag_zag_unit_test}];
    await runAllDatabases(abap, files, () => {
      expect(abap.console.get()).to.equal("1");
    });
  });

  it("FOR ALL ENTRIES, two level", async () => {
    const code = `
DATA lt_t100 TYPE STANDARD TABLE OF t100 WITH DEFAULT KEY.
DATA: BEGIN OF data_work,
        it_content TYPE STANDARD TABLE OF t100 WITH DEFAULT KEY,
      END OF data_work.
DATA row TYPE t100.
row-arbgb = 'ZAG_UNIT_TEST'.
APPEND row TO data_work-it_content.
SELECT *
  FROM t100
  INTO TABLE lt_t100
  FOR ALL ENTRIES IN data_work-it_content
  WHERE arbgb EQ data_work-it_content-arbgb.
WRITE sy-dbcnt.`;
    const files = [
      {filename: "zfoobar.prog.abap", contents: code},
      {filename: "t100.tabl.xml", contents: tabl_t100xml},
      {filename: "zag_unit_test.msag.xml", contents: msag_zag_unit_test}];
    await runAllDatabases(abap, files, () => {
      expect(abap.console.get()).to.equal("2");
    });
  });

  it("FOR ALL ENTRIES, duplicate results", async () => {
    const code = `
DATA input TYPE STANDARD TABLE OF t100-arbgb WITH DEFAULT KEY.
DATA result TYPE STANDARD TABLE OF t100 WITH DEFAULT KEY.
APPEND 'ZAG_UNIT_TEST' TO input.
APPEND 'ZAG_UNIT_TEST' TO input.
APPEND 'ZAG_UNIT_TEST' TO input.
APPEND 'ZAG_UNIT_TEST' TO input.
SELECT * FROM t100
  INTO TABLE result
  FOR ALL ENTRIES IN input
  WHERE sprsl = 'E'
  AND arbgb = input-table_line.
WRITE sy-dbcnt.`;
    const files = [
      {filename: "zfoobar.prog.abap", contents: code},
      {filename: "t100.tabl.xml", contents: tabl_t100xml},
      {filename: "zag_unit_test.msag.xml", contents: msag_zag_unit_test}];
    await runAllDatabases(abap, files, () => {
      expect(abap.console.get()).to.equal("2");
    });
  });

  it("SELECT LOOP CORRESPONDING", async () => {
    const code = `
DATA: BEGIN OF res,
        arbgb TYPE t100-arbgb,
        something TYPE i,
      END OF RES.
SELECT arbgb
  INTO CORRESPONDING FIELDS OF res
  FROM t100
  UP TO 1 ROWS.
ENDSELECT.
WRITE res-arbgb.`;
    const files = [
      {filename: "zfoobar.prog.abap", contents: code},
      {filename: "t100.tabl.xml", contents: tabl_t100xml},
      {filename: "zag_unit_test.msag.xml", contents: msag_zag_unit_test}];
    await runAllDatabases(abap, files, () => {
      expect(abap.console.get().trimEnd()).to.equal("ZAG_UNIT_TEST");
    });
  });

  it("SINGLE ``", async () => {
    const code = `
DATA: BEGIN OF res,
        arbgb TYPE t100-arbgb,
        something TYPE i,
      END OF RES.
SELECT SINGLE arbgb
  INTO CORRESPONDING FIELDS OF res
  FROM t100
  WHERE arbgb = \`ZAG_UNIT_TEST\`.
WRITE res-arbgb.`;
    const files = [
      {filename: "zfoobar.prog.abap", contents: code},
      {filename: "t100.tabl.xml", contents: tabl_t100xml},
      {filename: "zag_unit_test.msag.xml", contents: msag_zag_unit_test}];
    await runAllDatabases(abap, files, () => {
      expect(abap.console.get().trimEnd()).to.equal("ZAG_UNIT_TEST");
    });
  });

  it("SELECT into hashed", async () => {
    const code = `
DATA hashed TYPE HASHED TABLE OF t100 WITH UNIQUE KEY sprsl arbgb msgnr.
SELECT * FROM t100 INTO TABLE hashed.
WRITE sy-dbcnt.`;
    const files = [
      {filename: "zfoobar.prog.abap", contents: code},
      {filename: "t100.tabl.xml", contents: tabl_t100xml},
      {filename: "zag_unit_test.msag.xml", contents: msag_zag_unit_test}];
    await runAllDatabases(abap, files, () => {
      expect(abap.console.get()).to.equal("2");
    });
  });

  it("FOR ALL ENTRIES, into HASHED", async () => {
    const code = `
    DATA lt_t100 TYPE HASHED TABLE OF t100 WITH UNIQUE KEY sprsl arbgb msgnr.
    DATA lt_fae TYPE STANDARD TABLE OF t100 WITH DEFAULT KEY.
    DATA ls_fae LIKE LINE OF lt_fae.

    ls_fae-msgnr = '123'.
    INSERT ls_fae INTO TABLE lt_fae.

    SELECT * FROM t100 INTO TABLE lt_t100
      FOR ALL ENTRIES IN lt_fae
      WHERE msgnr = lt_fae-msgnr.
    WRITE sy-dbcnt.`;
    const files = [
      {filename: "zfoobar.prog.abap", contents: code},
      {filename: "t100.tabl.xml", contents: tabl_t100xml},
      {filename: "zag_unit_test.msag.xml", contents: msag_zag_unit_test}];
    await runAllDatabases(abap, files, () => {
      expect(abap.console.get()).to.equal("1");
    });
  });

  it("SELECT into list of basic", async () => {
    const code = `
    DATA lv_arbgb TYPE t100-arbgb.
    DATA lv_text TYPE t100-text.

    SELECT SINGLE arbgb text INTO (lv_arbgb, lv_text) FROM t100.
    WRITE / lv_arbgb.
    WRITE / lv_text.`;
    const files = [
      {filename: "zfoobar.prog.abap", contents: code},
      {filename: "t100.tabl.xml", contents: tabl_t100xml},
      {filename: "zag_unit_test.msag.xml", contents: msag_zag_unit_test}];
    await runAllDatabases(abap, files, () => {
      expect(abap.console.get().trimEnd()).to.equal("ZAG_UNIT_TEST       \nhello world");
    });
  });

  it("INSERT dynamic", async () => {
    const code = `
    DATA lv_name type c length 30.
    DATA tab TYPE STANDARD TABLE OF t100 WITH DEFAULT KEY.
    lv_name = 'T100'.
    APPEND INITIAL LINE TO tab.
    INSERT (lv_name) FROM TABLE tab.
    WRITE sy-subrc.`;
    const files = [
      {filename: "zfoobar.prog.abap", contents: code},
      {filename: "t100.tabl.xml", contents: tabl_t100xml},
      {filename: "zag_unit_test.msag.xml", contents: msag_zag_unit_test}];
    await runAllDatabases(abap, files, () => {
      expect(abap.console.get().trimEnd()).to.equal("0");
    });
  });

  it("SELECT LOOP PACKAGE SIZE", async () => {
    const code = `
DATA lt TYPE STANDARD TABLE OF t100 WITH DEFAULT KEY.
SELECT arbgb msgnr
    INTO CORRESPONDING FIELDS OF TABLE lt
    FROM t100
    PACKAGE SIZE 1000.
  WRITE / lines( lt ).
ENDSELECT.`;
    const files = [
      {filename: "zfoobar.prog.abap", contents: code},
      {filename: "t100.tabl.xml", contents: tabl_t100xml},
      {filename: "zag_unit_test.msag.xml", contents: msag_zag_unit_test}];
    await runAllDatabases(abap, files, () => {
      expect(abap.console.get().trimEnd()).to.equal("2");
    });
  });

  it("SINGLE star into CORRESPONDING FIELDS OF", async () => {
    const code = `
DATA: BEGIN OF res,
        arbgb TYPE t100-arbgb,
        something TYPE i,
      END OF RES.
SELECT SINGLE *
  INTO CORRESPONDING FIELDS OF res
  FROM t100
  WHERE arbgb = \`ZAG_UNIT_TEST\`.
WRITE res-arbgb.`;
    const files = [
      {filename: "zfoobar.prog.abap", contents: code},
      {filename: "t100.tabl.xml", contents: tabl_t100xml},
      {filename: "zag_unit_test.msag.xml", contents: msag_zag_unit_test}];
    await runAllDatabases(abap, files, () => {
      expect(abap.console.get().trimEnd()).to.equal("ZAG_UNIT_TEST");
    });
  });

  it("SELECT INTO TABLE, ORDER BY PRIMARY KEY, dynamic variable", async () => {
    const code = `
    DATA tab TYPE STANDARD TABLE OF t100 WITH DEFAULT KEY.
    DATA name TYPE c LENGTH 30.
    name = 'T100'.
    SELECT * FROM (name) INTO TABLE tab ORDER BY PRIMARY KEY.
    WRITE sy-dbcnt.`;
    const files = [
      {filename: "zfoobar.prog.abap", contents: code},
      {filename: "t100.tabl.xml", contents: tabl_t100xml},
      {filename: "zag_unit_test.msag.xml", contents: msag_zag_unit_test}];
    await runAllDatabases(abap, files, () => {
      expect(abap.console.get()).to.equal("2");
    });
  });

});