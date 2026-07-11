import {expect} from "chai";
import {ABAP, MemoryConsole} from "../../packages/runtime/src";
import {AsyncFunction, runFiles} from "../_utils";

let abap: ABAP;

async function run(contents: string) {
  return runFiles(abap, [{filename: "zfoobar.prog.abap", contents}]);
}

describe("Running statements - WAIT", () => {

  beforeEach(async () => {
    abap = new ABAP({console: new MemoryConsole()});
  });

  it("sets sy-subrc to zero when the condition is true", async () => {
    const code = `
DATA foo TYPE i.
foo = 1.
sy-subrc = 4.
WAIT UNTIL foo = 1 UP TO 1 SECONDS.
WRITE sy-subrc.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("0");
  });

  it("times out and sets sy-subrc to eight", async () => {
    const code = `
DATA foo TYPE i.
WAIT UNTIL foo = 1 UP TO 1 SECONDS.
WRITE sy-subrc.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    const start = Date.now();
    await f(abap);
    const elapsed = Date.now() - start;

    expect(abap.console.get()).to.equal("8");
    expect(elapsed).to.be.lessThan(250);
  });

});
