import {expect} from "chai";
import {ABAP, MemoryConsole} from "../../packages/runtime/src";
import {AsyncFunction, runFiles} from "../_utils";

let abap: ABAP;

async function run(contents: string) {
  return runFiles(abap, [{filename: "zfoobar.prog.abap", contents}]);
}

describe("Running statements - WHILE", () => {

  beforeEach(async () => {
    abap = new ABAP({console: new MemoryConsole()});
  });

  it("simple", async () => {
    const code = `
DATA i TYPE i.
WHILE i < 4.
  WRITE / sy-index.
  i = i + 1.
ENDWHILE.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("1\n2\n3\n4");
  });

  it("reset sytabix", async () => {
    const code = `
DATA i TYPE i.
sy-index = 10.
WHILE i < 5.
  WRITE / sy-index.
  i = i + 1.
ENDWHILE.
WRITE / sy-index.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("1\n2\n3\n4\n5\n10");
  });

  it("numc while", async () => {
    const code = `
CONSTANTS lc_size TYPE i VALUE 100.
DATA lv_offset TYPE n LENGTH 5.

WHILE lv_offset < 500.
  lv_offset = lv_offset + lc_size.
  WRITE / lv_offset.
ENDWHILE.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal(`00100
00200
00300
00400
00500`);
  });

  it("DATA declared inside WHILE body is not re-initialized", async () => {
    const code = `
TYPES ty_ints TYPE STANDARD TABLE OF i WITH DEFAULT KEY.
DATA lv_index TYPE i.
WHILE lv_index < 3.
  lv_index = lv_index + 1.
  DATA lt_arguments TYPE ty_ints.
  APPEND lv_index TO lt_arguments.
  ASSERT lines( lt_arguments ) = lv_index.
ENDWHILE.
ASSERT lines( lt_arguments ) = 3.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
  });

});