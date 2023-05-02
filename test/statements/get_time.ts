import {expect} from "chai";
import {ABAP} from "../../packages/runtime/src/index.js";
import {AsyncFunction, runFiles} from "../_utils.js";

let abap: ABAP;

async function run(contents: string) {
  return runFiles(abap, [{filename: "zfoobar.prog.abap", contents}]);
}

describe("Running statements - GET TIME", () => {

  beforeEach(async () => {
    abap = new ABAP();
  });

  it("GET TIME", async () => {
    const code = `
    GET TIME.
    WRITE / sy-datlo.
    WRITE / sy-datum.
    WRITE / sy-timlo.
    WRITE / sy-uzeit.`;

    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.not.equal("");
  });

  it("GET TIME FIELD", async () => {
    const code = `
    DATA bar TYPE t.
    GET TIME FIELD bar.
    ASSERT NOT bar IS INITIAL.`;

    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
  });

  it("GET TIME STAMP FIELD", async () => {
    const code = `
    DATA bar TYPE p.
    GET TIME STAMP FIELD bar.
    WRITE |{ bar TIMESTAMP = ISO }|.
    ASSERT NOT bar IS INITIAL.`;

    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
//    console.dir(abap.console.get());
  });

});