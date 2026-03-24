import {expect} from "chai";
import {ABAP, MemoryConsole} from "../../packages/runtime/src/";
import {AsyncFunction, runFiles} from "../_utils";

let abap: ABAP;

async function run(contents: string) {
  return runFiles(abap, [{filename: "zfoobar.prog.abap", contents}]);
}

describe("Running Examples - ENUMs", () => {

  beforeEach(async () => {
    abap = new ABAP({console: new MemoryConsole()});
  });

  it("basics", async () => {
    const code = `
TYPES: BEGIN OF ENUM ty_cache_policy STRUCTURE cache_policies,
         use_all,
         use_none,
       END OF ENUM ty_cache_policy STRUCTURE cache_policies.

DATA foo TYPE ty_cache_policy.
WRITE / cache_policies-use_all.
WRITE / foo.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("USE_ALL\nUSE_ALL");
  });

});