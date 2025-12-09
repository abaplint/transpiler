import {expect} from "chai";
import {ABAP, MemoryConsole} from "../../packages/runtime/src";
import {AsyncFunction, runFiles} from "../_utils";

let abap: ABAP;

async function run(contents: string) {
  return runFiles(abap, [{filename: "zfoobar.prog.abap", contents}]);
}

describe("Running statements - GET TIME", () => {

  beforeEach(async () => {
    abap = new ABAP({console: new MemoryConsole()});
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

  it("GET TIME STAMP FIELD, high resolution", async () => {
    const code = `
  DATA lv TYPE p LENGTH 11 DECIMALS 7.
  GET TIME STAMP FIELD lv.
  WRITE / lv.`;

    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
//    console.dir(abap.console.get());
  });

});