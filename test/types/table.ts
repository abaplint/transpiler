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

  it("Basic, sorted", async () => {
    const code = `
  DATA tab TYPE SORTED TABLE OF i WITH UNIQUE KEY table_line.
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
    expect(abap.console.get()).to.equal("1\n2\n3");
  });

  it("sorted, already exists", async () => {
    const code = `
  DATA tab TYPE SORTED TABLE OF i WITH UNIQUE KEY table_line.
  DATA row LIKE LINE OF tab.
  INSERT 1 INTO TABLE tab.
  WRITE / sy-subrc.
  INSERT 1 INTO TABLE tab.
  WRITE / sy-subrc.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("0\n4");
  });

  it.skip("sorted, duplicate key, expect dump", async () => {
    const code = `
  DATA tab TYPE SORTED TABLE OF i WITH UNIQUE KEY table_line.
  DATA row LIKE LINE OF tab.
  APPEND 1 TO tab.
  APPEND 1 TO tab.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    expect(async () => f(abap)).to.throw();
  });

  it.skip("sorted, structured", async () => {
    const code = `
TYPES: BEGIN OF type,
         int  TYPE i,
         char TYPE c LENGTH 4,
       END OF type.
DATA tab TYPE SORTED TABLE OF type WITH UNIQUE KEY int char.
DATA row LIKE LINE OF tab.
CLEAR row.
row-int = 1.
row-char = 'BBBB'.
INSERT row INTO TABLE tab.
CLEAR row.
row-int = 1.
row-char = 'AAAA'.
INSERT row INTO TABLE tab.
LOOP AT tab INTO row.
  WRITE / row-char.
ENDLOOP.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    expect(async () => f(abap)).to.throw();
  });

});