// import {expect} from "chai";
import {ABAP, MemoryConsole} from "../../packages/runtime/src";
import {AsyncFunction, runFiles} from "../_utils";

let abap: ABAP;

async function run(contents: string) {
  return runFiles(abap, [{filename: "zfoobar.prog.abap", contents}]);
}

describe("Running operators - BIT-NOT", () => {

  beforeEach(async () => {
    abap = new ABAP({console: new MemoryConsole()});
  });

  it("test 1", async () => {
    const code = `
    DATA val TYPE x LENGTH 8.
    val = 'FFFFFFFFFFFFFFFF'.
    val = BIT-NOT val.
    ASSERT val = '0000000000000000'.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
  });

  it("test 2", async () => {
    const code = `
    DATA val TYPE x LENGTH 8.
    val = 'FFFFFFFF00000000'.
    val = BIT-NOT val.
    ASSERT val = '00000000FFFFFFFF'.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
  });

});