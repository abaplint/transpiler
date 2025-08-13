// import {expect} from "chai";
import {ABAP, MemoryConsole} from "../../packages/runtime/src";
import {AsyncFunction, runFiles} from "../_utils";

let abap: ABAP;

async function run(contents: string, skipVersionCheck = false) {
  return runFiles(abap, [{filename: "zfoobar_xsdbool.prog.abap", contents}], {skipVersionCheck});
}

describe("Builtin functions - xsdbool", () => {

  beforeEach(async () => {
    abap = new ABAP({console: new MemoryConsole()});
  });

  it("xsdbool true", async () => {
    const code = `ASSERT xsdbool( 1 = 1 ) = abap_true.`;
    const js = await run(code, true);
    const f = new AsyncFunction("abap", js);
    await f(abap);
  });

  it("xsdbool false", async () => {
    const code = `ASSERT xsdbool( 1 = 2 ) = abap_false.`;
    const js = await run(code, true);
    const f = new AsyncFunction("abap", js);
    await f(abap);
  });

});
