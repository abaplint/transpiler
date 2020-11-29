import {expect} from "chai";
import {runFiles} from "./_utils";
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
    const f = new Function("abap", js);
    f(abap);
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
    const f = new Function("abap", js);
    f(abap);
    expect(abap.console.get()).to.equal("4");
  });

});