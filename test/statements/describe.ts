import {expect} from "chai";
import {ABAP} from "../../packages/runtime/src";
import {AsyncFunction, runFiles} from "../_utils";

let abap: ABAP;

async function run(contents: string) {
  return runFiles(abap, [{filename: "zfoobar.prog.abap", contents}]);
}

describe("Running statements - DESCRIBE", () => {

  beforeEach(async () => {
    abap = new ABAP();
  });

  it("DESCRIBE with field symbol", async () => {
    const code = `
    DATA lv_length TYPE i.
    DATA bar TYPE c LENGTH 10.
    FIELD-SYMBOLS <line> TYPE any.
    ASSIGN bar TO <line>.
    DESCRIBE FIELD <line> LENGTH lv_length IN CHARACTER MODE.
    WRITE lv_length.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("10");
  });

  it("more DESCRIBE", async () => {
    const code = `
    DATA lv_type TYPE c LENGTH 1.
    DESCRIBE FIELD lv_type TYPE lv_type.
    WRITE / lv_type.

    DATA lv_string TYPE string.
    DESCRIBE FIELD lv_string TYPE lv_type.
    WRITE / lv_type.

    DATA lt_tab TYPE STANDARD TABLE OF string.
    DESCRIBE FIELD lt_tab TYPE lv_type.
    WRITE / lv_type.

    DATA ref TYPE REF TO object.
    DESCRIBE FIELD ref TYPE lv_type.
    WRITE / lv_type.`;

    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("C\ng\nh\nr");
  });

  it("DESCRIBE, direct character string", async () => {
    const code = `
  DATA lv_type TYPE c LENGTH 1.
  DESCRIBE FIELD 'moo' TYPE lv_type.
  ASSERT lv_type = 'C'.`;

    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
  });

  it("DESCRIBE FIELD table", async () => {
    const code = `
  DATA tab TYPE STANDARD TABLE OF i WITH DEFAULT KEY.
  DATA type TYPE c LENGTH 1.
  DESCRIBE FIELD tab TYPE type.
  WRITE type.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("h");
  });

  it("DESCRIBE FIELD 1", async () => {
    const code = `
  DATA f TYPE c LENGTH 4.
  DATA l TYPE i.
  DESCRIBE FIELD f LENGTH l IN CHARACTER MODE.
  WRITE l.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("4");
  });

  it("DESCRIBE FIELD 2", async () => {
    const code = `
    DATA tab TYPE STANDARD TABLE OF string WITH DEFAULT KEY.
    DATA bar TYPE string.
    DATA type TYPE c LENGTH 1.
    APPEND 'foo' TO tab.
    READ TABLE tab INDEX 1 INTO bar.
    DESCRIBE FIELD bar TYPE type.
    ASSERT type = 'g'.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
  });

  it("DESCRIBE FIELD 3", async () => {
    const code = `
  DATA tab TYPE STANDARD TABLE OF string WITH DEFAULT KEY.
  FIELD-SYMBOLS <bar> TYPE any.
  DATA type TYPE c LENGTH 1.
  APPEND 'foo' TO tab.
  READ TABLE tab INDEX 1 ASSIGNING <bar>.
  DESCRIBE FIELD <bar> TYPE type.
  ASSERT type = 'g'.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
  });

});