import {expect} from "chai";
import {ABAP} from "../../packages/runtime/src/";
import {AsyncFunction, runFiles} from "../_utils";

let abap: ABAP;

async function run(contents: string) {
  return runFiles(abap, [{filename: "zfoobar.prog.abap", contents}]);
}

describe("Running Examples - Internal table type", () => {

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

  it("sorted, structured", async () => {
    const code = `
TYPES: BEGIN OF type,
         foo TYPE i,
         bar TYPE c LENGTH 4,
       END OF type.
DATA tab TYPE SORTED TABLE OF type WITH UNIQUE KEY foo bar.
DATA row LIKE LINE OF tab.
CLEAR row.
row-foo = 1.
row-bar = 'BBBB'.
INSERT row INTO TABLE tab.
CLEAR row.
row-foo = 1.
row-bar = 'AAAA'.
INSERT row INTO TABLE tab.
LOOP AT tab INTO row.
  WRITE / row-bar.
ENDLOOP.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("AAAA\nBBBB");
  });

  it("copying table to sorted table should sort", async () => {
    const code = `
  DATA tab TYPE STANDARD TABLE OF i WITH DEFAULT KEY.
  DATA sorted TYPE SORTED TABLE OF i WITH UNIQUE KEY table_line.
  DATA row LIKE LINE OF tab.
  INSERT 1 INTO TABLE tab.
  INSERT 3 INTO TABLE tab.
  INSERT 2 INTO TABLE tab.
  sorted = tab.
  LOOP AT sorted INTO row.
    WRITE / row.
  ENDLOOP.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("1\n2\n3");
  });

  it.skip("complex key", async () => {
    const code = `
  TYPES:
    BEGIN OF ty_node,
      path TYPE string,
      name TYPE string,
      type TYPE string,
      value TYPE string,
      index TYPE i,
      order TYPE i,
      children TYPE i,
    END OF ty_node .
  TYPES:
    ty_nodes_tt TYPE STANDARD TABLE OF ty_node WITH KEY path name .
  TYPES:
    ty_nodes_ts TYPE SORTED TABLE OF ty_node
      WITH UNIQUE KEY path name
      WITH NON-UNIQUE SORTED KEY array_index COMPONENTS path index
      WITH NON-UNIQUE SORTED KEY item_order COMPONENTS path order .`;
    const js = await run(code);
    console.dir(js);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("1\n2\n3");
  });

});