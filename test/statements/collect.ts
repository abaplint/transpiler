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

});