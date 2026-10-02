import {expect} from "chai";
import {ABAP, MemoryConsole} from "../../packages/runtime/src";
import {AsyncFunction, runFiles} from "../_utils";

let abap: ABAP;

async function run(contents: string, skipVersionCheck = false) {
  return runFiles(abap, [{filename: "zfoobar_perform.prog.abap", contents}], {skipVersionCheck});
}

describe("Running statements - PERFORM", () => {

  beforeEach(async () => {
    abap = new ABAP({console: new MemoryConsole()});
  });

  it("PERFORM IF FOUND", async () => {
    const code = `
PERFORM sdfsdf IN PROGRAM sdfsdf IF FOUND.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("");
  });

  it("PERFORM IN PROGRAM, initial dynamic name, IF FOUND", async () => {
    const code = `
DATA lv_prog TYPE c LENGTH 40.
PERFORM hello IN PROGRAM (lv_prog) IF FOUND.
WRITE 'after'.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("after");
  });

  it("PERFORM IN PROGRAM, initial dynamic name, raises CX_SY_DYN_CALL_ILLEGAL_FORM", async () => {
    class IllegalForm extends Error {
      public async constructor_() {
        return this;
      }
    }
    abap.Classes["CX_SY_DYN_CALL_ILLEGAL_FORM"] = IllegalForm;
    const code = `
DATA lv_prog TYPE c LENGTH 40.
PERFORM hello IN PROGRAM (lv_prog).
WRITE 'after'.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    let caught: any;
    try {
      await f(abap);
    } catch (error) {
      caught = error;
    }
    expect(caught).to.be.instanceof(IllegalForm);
    expect(abap.console.get()).to.equal("");
  });

  it("PERFORM IN PROGRAM, unknown static name, raises CX_SY_DYN_CALL_ILLEGAL_FORM", async () => {
    class IllegalForm extends Error {
      public async constructor_() {
        return this;
      }
    }
    abap.Classes["CX_SY_DYN_CALL_ILLEGAL_FORM"] = IllegalForm;
    const code = `PERFORM hello IN PROGRAM zunknown.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    let caught: any;
    try {
      await f(abap);
    } catch (error) {
      caught = error;
    }
    expect(caught).to.be.instanceof(IllegalForm);
  });

});
