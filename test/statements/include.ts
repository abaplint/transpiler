import {expect} from "chai";
import {ABAP, MemoryConsole} from "../../packages/runtime/src";
import {AsyncFunction, runFiles} from "../_utils";

let abap: ABAP;

describe.only("Running statements - INCLUDE", () => {

  beforeEach(async () => {
    abap = new ABAP({console: new MemoryConsole()});
  });

  it("simple", async () => {
    const zincl = `
DATA foo TYPE i.
foo = 2.`;
    const top = `
INCLUDE zincl.
WRITE foo.`;

    const js = await runFiles(abap, [{filename: "zfoobar.prog.abap", contents: top}, {filename: "zincl.prog.abap", contents: zincl}]);
    console.dir(js);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("2");
  });

});