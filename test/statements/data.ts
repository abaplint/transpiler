// import {expect} from "chai";
import {ABAP} from "../../packages/runtime/src";
import {AsyncFunction, runFiles} from "../_utils";

let abap: ABAP;

async function run(contents: string) {
  return runFiles(abap, [{filename: "zfoobar.prog.abap", contents}]);
}

describe("Running statements - DATA", () => {

  beforeEach(async () => {
    abap = new ABAP();
  });

  it("DATA, with BEGIN OF", async () => {
    const code = `
DATA: BEGIN OF ls_msg,
        a1 TYPE c LENGTH 1,
        a2 TYPE c LENGTH 1,
        a3 TYPE c LENGTH 1,
        a4 TYPE c LENGTH 1,
      END OF ls_msg.
ls_msg-a3 = 'A'.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
  });

  it("DATA, upper case component name", async () => {
    const code = `
DATA: BEGIN OF ls_msg,
        a3 TYPE c LENGTH 1,
      END OF ls_msg.
ls_msg-a3 = 'A'.
ls_msg-A3 = 'A'.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
  });

});