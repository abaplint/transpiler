import {expect} from "chai";
import {ABAP, MemoryConsole} from "../../packages/runtime/src";
import {AsyncFunction, runFiles} from "../_utils";

let abap: ABAP;

async function run(contents: string) {
  return runFiles(abap, [{filename: "zfoobar.prog.abap", contents}]);
}

describe("Running code structure - FORM / PERFORM", () => {

  beforeEach(async () => {
    abap = new ABAP({console: new MemoryConsole()});
  });

  it("basic", async () => {
    const code = `
    FORM hello.
      WRITE / 'hello'.
    ENDFORM.

    START-OF-SELECTION.
      PERFORM hello.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("hello");
  });

  it("with input", async () => {
    const code = `
    FORM hello USING bar TYPE string.
      WRITE / bar.
    ENDFORM.

    START-OF-SELECTION.
      PERFORM hello USING 'hello'.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("hello");
  });

  it("CHANGING parameter", async () => {
    const code = `
    FORM double CHANGING cv_val TYPE i.
      cv_val = cv_val * 2.
    ENDFORM.

    START-OF-SELECTION.
      DATA lv_int TYPE i VALUE 21.
      PERFORM double CHANGING lv_int.
      WRITE / lv_int.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("42");
  });

  it("USING and CHANGING together", async () => {
    const code = `
    FORM concat USING iv_in TYPE string CHANGING cv_out TYPE string.
      CONCATENATE cv_out iv_in INTO cv_out.
    ENDFORM.

    START-OF-SELECTION.
      DATA lv_str TYPE string VALUE 'foo'.
      PERFORM concat USING 'bar' CHANGING lv_str.
      WRITE / lv_str.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("foobar");
  });

});
