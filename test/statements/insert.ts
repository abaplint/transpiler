import {expect} from "chai";
import {ABAP} from "../../packages/runtime/src";
import {AsyncFunction, runFiles} from "../_utils";

let abap: ABAP;

async function run(contents: string) {
  return runFiles(abap, [{filename: "zfoobar.prog.abap", contents}]);
}

describe("Running statements - INSERT", () => {

  beforeEach(async () => {
    abap = new ABAP();
  });

  it("INSERT INTO TABLE", async () => {
    const code = `
      DATA tab TYPE STANDARD TABLE OF i WITH DEFAULT KEY.
      INSERT 5 INTO TABLE tab.
      ASSERT lines( tab ) = 1.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
  });

  it("INSERT INDEX, one time before loop pointer", async () => {
    const code = `
      DATA tab TYPE STANDARD TABLE OF i WITH DEFAULT KEY.
      DATA row LIKE LINE OF tab.
      DO 3 TIMES.
        APPEND sy-index TO tab.
      ENDDO.
      LOOP AT tab INTO row.
        WRITE / row.
        IF row MOD 2 = 0.
          ASSERT lines( tab ) < 10.
          INSERT 5 INTO tab INDEX sy-tabix.
        ENDIF.
      ENDLOOP.
      ASSERT lines( tab ) = 4.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("1\n2\n3");
  });

  it("INSERT INDEX, with SORT", async () => {
    const code = `
      DATA tab TYPE STANDARD TABLE OF i WITH DEFAULT KEY.
      DATA row LIKE LINE OF tab.
      DO 4 TIMES.
        row = 5 - sy-index.
        APPEND row TO tab.
      ENDDO.
      LOOP AT tab INTO row.
        WRITE / row.
        SORT tab.
      ENDLOOP.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("4\n2\n3\n4");
  });

  it("INSERT INTO TABLE 2", async () => {
    const code = `
      DATA bar TYPE STANDARD TABLE OF i WITH DEFAULT KEY.
      DATA data LIKE LINE OF bar.
      INSERT 1 INTO TABLE bar.
      INSERT 2 INTO TABLE bar.
      LOOP AT bar INTO data.
        WRITE / data.
      ENDLOOP.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("1\n2");
  });

  it("INSERT ASSIGNING", async () => {
    const code = `
      DATA tab TYPE TABLE OF i.
      FIELD-SYMBOLS <i> TYPE i.
      INSERT 7 INTO TABLE tab ASSIGNING <i>.
      ASSERT <i> = 7.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
  });

  it("INSERT LINES OF", async () => {
    const code = `
  DATA tab1 TYPE STANDARD TABLE OF i WITH DEFAULT KEY.
  DATA tab2 TYPE STANDARD TABLE OF i WITH DEFAULT KEY.
  DATA val LIKE LINE OF tab1.
  APPEND 1 TO tab1.
  APPEND 2 TO tab2.
  INSERT LINES OF tab2 INTO TABLE tab1.
  LOOP AT tab1 INTO val.
    WRITE / val.
  ENDLOOP.
  ASSERT lines( tab1 ) = 2.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("1\n2");
  });

  it("INSERT INDEX", async () => {
    const code = `
  DATA tab TYPE STANDARD TABLE OF i WITH DEFAULT KEY.
  DATA int TYPE i.
  INSERT 1 INTO tab INDEX 1.
  INSERT 2 INTO tab INDEX 1.
  LOOP AT tab INTO int.
    WRITE / int.
  ENDLOOP.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("2\n1");
  });

  it("INSERT REFERENCE INTO", async () => {
    const code = `
DATA tab TYPE STANDARD TABLE OF i.
DATA ref TYPE REF TO i.
DATA ref2 TYPE REF TO i.
DATA int TYPE i.

int = 2.

INSERT int INTO TABLE tab REFERENCE INTO ref.
WRITE / ref->*.

READ TABLE tab INDEX 1 REFERENCE INTO ref2.
WRITE / ref2->*.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("2\n2");
  });

  it("INSERT string", async () => {
    const code = `
  DATA result TYPE STANDARD TABLE OF string WITH DEFAULT KEY.
  INSERT '' INTO TABLE result.
  ASSERT lines( result ) = 1.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
  });

  it("INSERT and references", async () => {
    const code = `
TYPES: BEGIN OF aggregated_data_type,
         group   TYPE i,
       END OF aggregated_data_type.
DATA aggregated_data TYPE STANDARD TABLE OF aggregated_data_type WITH DEFAULT KEY.
DATA temp1 LIKE LINE OF aggregated_data.
DATA aggregated LIKE REF TO temp1.
INSERT INITIAL LINE INTO TABLE aggregated_data REFERENCE INTO aggregated.
aggregated->group = 2.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
  });

  it("INSERT, unique dashed key", async () => {
    const code = `
TYPES: BEGIN OF ty_file,
         path     TYPE string,
         filename TYPE string,
       END OF ty_file.
TYPES: BEGIN OF ty_stage,
         file TYPE ty_file,
       END OF ty_stage .
DATA tab TYPE SORTED TABLE OF ty_stage WITH UNIQUE KEY file-path file-filename.
DATA row LIKE LINE OF tab.
row-file-path = '1'.
row-file-filename = '1'.
INSERT row INTO TABLE tab.
row-file-path = '1'.
row-file-filename = '2'.
INSERT row INTO TABLE tab.
WRITE lines( tab ).`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("2");
  });

});