import {expect} from "chai";
import {ABAP, MemoryConsole} from "../../packages/runtime/src";
import {AsyncFunction, runFiles} from "../_utils";

let abap: ABAP;

async function run(contents: string) {
  return runFiles(abap, [{filename: "zfoobar.prog.abap", contents}]);
}

describe("Running statements - GET PARAMETER", () => {

  beforeEach(async () => {
    abap = new ABAP({console: new MemoryConsole()});
  });

  it("simple", async () => {
    const code = `
    DATA lv_foo TYPE c LENGTH 10.
    GET PARAMETER ID 'ABC' FIELD lv_foo.
    WRITE sy-subrc.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("4");
  });

  it("SET PARAMETER, then GET PARAMETER", async () => {
    const code = `
    DATA lv_in TYPE c LENGTH 10 VALUE 'stored'.
    DATA lv_out TYPE c LENGTH 10.
    SET PARAMETER ID 'ZPID' FIELD lv_in.
    GET PARAMETER ID 'ZPID' FIELD lv_out.
    WRITE |{ sy-subrc }:{ lv_out }|.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("0:stored");
  });

  it("GET PARAMETER of a parameter never set clears the field", async () => {
    const code = `
    DATA lv_out TYPE c LENGTH 10 VALUE 'old'.
    GET PARAMETER ID 'ZNEVER' FIELD lv_out.
    WRITE |{ sy-subrc }:{ lv_out }.|.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("4:.");
  });

  it("parameter IDs are case sensitive", async () => {
    const code = `
    DATA lv_in TYPE c LENGTH 10 VALUE 'upper'.
    DATA lv_out TYPE c LENGTH 10.
    SET PARAMETER ID 'ZPID' FIELD lv_in.
    GET PARAMETER ID 'zpid' FIELD lv_out.
    WRITE |{ sy-subrc }:{ lv_out }.|.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("4:.");
  });

});