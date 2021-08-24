import {expect} from "chai";
import {ABAP} from "../../packages/runtime/src/";
import {AsyncFunction, runFiles} from "../_utils";

let abap: ABAP;

async function run(contents: string) {
  return runFiles(abap, [{filename: "zfoobar.prog.abap", contents}]);
}

describe("Running Examples - Table type", () => {

  beforeEach(async () => {
    abap = new ABAP();
  });

  it("Basic, non sorted", async () => {
    const code = `
    DATA tab TYPE STANDARD TABLE OF i WITH DEFAULT KEY.
    DATA row LIKE LINE OF tab.
    INSERT 1 INTO TABLE tab.
    INSERT 3 INTO TABLE tab.
    INSERT 2 INTO TABLE tab.
    LOOP AT tab INTO row.
      WRITE / row.
    ENDLOOP.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("1\n3\n2");
  });

  /*
  DATA tab TYPE SORTED TABLE OF i WITH UNIQUE KEY table_line.
  DATA row LIKE LINE OF tab.
  INSERT 1 INTO TABLE tab.
  INSERT 3 INTO TABLE tab.
  INSERT 2 INTO TABLE tab.
  LOOP AT tab INTO row.
    WRITE / row.
  ENDLOOP.
  */

});