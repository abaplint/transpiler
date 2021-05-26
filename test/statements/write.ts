import {expect} from "chai";
import {ABAP} from "../../packages/runtime/src";
import {AsyncFunction, runFiles} from "../_utils";

let abap: ABAP;

async function run(contents: string) {
  return runFiles(abap, [{filename: "zfoobar.prog.abap", contents}]);
}

describe("Running statements - WRITE", () => {

  beforeEach(async () => {
    abap = new ABAP();
  });

  it("WRITE - single line", async () => {
    const code = `
        WRITE /. 
    `;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("\n");
  });

  it("WRITE - structure", async () => {
    const code = `
      TYPES:
        BEGIN OF ts_str,
          comp_one TYPE c LENGTH 10,
        END OF ts_str.
        DATA ls_str TYPE ts_str.
        ls_str-comp_one = 'A'.
        write: ls_str.
    `;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("A");
  });


});