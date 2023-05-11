import {expect} from "chai";
import {ABAP, MemoryConsole} from "../../packages/runtime/src";
import {AsyncFunction, runFiles} from "../_utils";

let abap: ABAP;

async function run(contents: string) {
  return runFiles(abap, [{filename: "ipow.prog.abap", contents}]);
}

describe("Builtin functions - ipow", () => {

  beforeEach(async () => {
    abap = new ABAP(new MemoryConsole());
  });

  it("basic", async () => {
    const code = `DATA val TYPE i.
val = ipow( base = 4 exp = 3 ).
WRITE val.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("64");
  });

  it("larger", async () => {
    const code = `DATA val TYPE i.
val = ipow( base = 4 exp = 10 ).
WRITE val.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("1048576");
  });

});
