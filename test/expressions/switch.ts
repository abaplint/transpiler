import {expect} from "chai";
import {ABAP, MemoryConsole} from "../../packages/runtime/src";
import {AsyncFunction, runFiles} from "../_utils";

let abap: ABAP;

async function run(contents: string, skipVersionCheck = false) {
  return runFiles(abap, [{filename: "zfoobar_switch.prog.abap", contents}], {skipVersionCheck});
}

describe("Running expressions - SWITCH", () => {

  beforeEach(async () => {
    abap = new ABAP({console: new MemoryConsole()});
  });

  it("basic", async () => {
    const code = `
FORM foo.
  DATA(lv_name) = SWITCH string(
    'sdf' WHEN 'sdf' THEN 'foo' WHEN 'bar' THEN 'bar' ).
  WRITE / lv_name.
ENDFORM.

START-OF-SELECTION.
  PERFORM foo.`;
    const js = await run(code, true);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("foo");
  });

});