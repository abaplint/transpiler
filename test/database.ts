import {runFiles} from "./_utils";
import * as abap from "../packages/runtime/src/";
import {tabl_t100xml} from "./_data";

describe("Top level tests, Database", () => {

  it.only("SELECT, no result", async () => {
    const code = `
    DATA ls_result TYPE t100.
    SELECT SINGLE * FROM t100 INTO ls_result.`;
    const js = await runFiles([
      {filename: "zfoobar.prog.abap", contents: code},
      {filename: "t100.tabl.xml", contents: tabl_t100xml}]);
    const f = new Function("abap", js);
    f(abap);
  });

});