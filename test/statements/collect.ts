import {expect} from "chai";
import {ABAP, MemoryConsole} from "../../packages/runtime/src";
import {AsyncFunction, runFiles} from "../_utils";

let abap: ABAP;

async function run(contents: string) {
  return runFiles(abap, [{filename: "zfoobar.prog.abap", contents}]);
}

describe("Running statements - COLLECT", () => {

  beforeEach(async () => {
    abap = new ABAP({console: new MemoryConsole()});
  });

  it("simple", async () => {
    const code = `
DATA lt_namespace TYPE STANDARD TABLE OF string WITH DEFAULT KEY.
DATA lv_namespace LIKE LINE OF lt_namespace.
lv_namespace = 'foo'.
COLLECT lv_namespace INTO lt_namespace.
lv_namespace = 'bar'.
COLLECT lv_namespace INTO lt_namespace.
lv_namespace = 'foo'.
COLLECT lv_namespace INTO lt_namespace.
LOOP AT lt_namespace INTO lv_namespace.
  WRITE / lv_namespace.
ENDLOOP.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("foo\nbar");
  });

  it("with header line", async () => {
    const code = `
DATA lt_namespace TYPE STANDARD TABLE OF string WITH HEADER LINE.
DATA lv_namespace LIKE LINE OF lt_namespace.
lt_namespace = 'foo'.
COLLECT lt_namespace.
LOOP AT lt_namespace INTO lv_namespace.
  WRITE / lv_namespace.
ENDLOOP.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("foo");
  });

  it("sums numeric components using the standard default key", async () => {
    const code = `
TYPES: BEGIN OF ty_line,
         id TYPE c LENGTH 4,
         amount TYPE i,
       END OF ty_line.
DATA lt_lines TYPE STANDARD TABLE OF ty_line WITH DEFAULT KEY.
DATA ls_line TYPE ty_line.
ls_line-id = 'P001'.
ls_line-amount = 1.
COLLECT ls_line INTO lt_lines.
COLLECT ls_line INTO lt_lines.
ls_line-amount = 2.
COLLECT ls_line INTO lt_lines.
WRITE / lines( lt_lines ).
LOOP AT lt_lines INTO ls_line.
  WRITE / ls_line-amount.
ENDLOOP.`;
    const js = await run(code);
    await new AsyncFunction("abap", js)(abap);
    expect(abap.console.get()).to.equal("1\n4");
  });

  it("sums a numeric table line with an empty default key", async () => {
    const code = `
DATA lt_values TYPE STANDARD TABLE OF i WITH DEFAULT KEY.
DATA lv_value TYPE i VALUE 1.
COLLECT lv_value INTO lt_values.
COLLECT lv_value INTO lt_values.
WRITE / lines( lt_values ).
LOOP AT lt_values INTO lv_value.
  WRITE / lv_value.
ENDLOOP.`;
    const js = await run(code);
    await new AsyncFunction("abap", js)(abap);
    expect(abap.console.get()).to.equal("1\n2");
  });

  for (const tableType of ["SORTED", "HASHED"]) {
    it(`sums numeric components in a ${tableType.toLowerCase()} table`, async () => {
      const code = `
TYPES: BEGIN OF ty_line,
         id TYPE c LENGTH 4,
         amount TYPE i,
       END OF ty_line.
DATA lt_lines TYPE ${tableType} TABLE OF ty_line WITH UNIQUE KEY id.
DATA ls_line TYPE ty_line.
ls_line-id = 'P001'.
ls_line-amount = 1.
COLLECT ls_line INTO lt_lines.
COLLECT ls_line INTO lt_lines.
WRITE / lines( lt_lines ).
LOOP AT lt_lines INTO ls_line.
  WRITE / ls_line-amount.
ENDLOOP.`;
      const js = await run(code);
      await new AsyncFunction("abap", js)(abap);
      expect(abap.console.get()).to.equal("1\n2");
    });
  }

});
