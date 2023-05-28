// import {expect} from "chai";
import {ABAP, MemoryConsole} from "../../packages/runtime/src";
import {AsyncFunction, runFiles} from "../_utils";

let abap: ABAP;

async function run(contents: string) {
  return runFiles(abap, [{filename: "zfoobar.prog.abap", contents}]);
}

describe("Builtin functions - to_upper", () => {

  beforeEach(async () => {
    abap = new ABAP({console: new MemoryConsole()});
  });

  it("to_upper()", async () => {
    const code = `
      DATA bar TYPE string VALUE 'BAR'.
      ASSERT to_upper( |bar| ) = |BAR|.
      ASSERT to_upper( |bar| ) = bar.
      ASSERT to_upper( bar ) = bar.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
  });

  it("test", async () => {
    const code = `
    DATA foo TYPE c LENGTH 40.
    foo = 'SDF'.
    ASSERT foo = to_upper( foo ).`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
  });

});
