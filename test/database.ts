import {expect} from "chai";
import {AsyncFunction, runFiles} from "./_utils";
import {ABAP} from "../packages/runtime/src/";
import {msag_zag_unit_test, tabl_t100xml} from "./_data";

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

});