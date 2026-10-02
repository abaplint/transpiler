import {expect} from "chai";
import {ABAP, MemoryConsole} from "../../packages/runtime/src";
import {AsyncFunction, runFiles} from "../_utils";

let abap: ABAP;

async function run(contents: string, skipVersionCheck = false) {
  return runFiles(abap, [{filename: "zfoobar_perform.prog.abap", contents}], {skipVersionCheck});
}

async function runProgram(filename: string, contents: string) {
  const js = await runFiles(abap, [{filename, contents}]);
  await new AsyncFunction("abap", js)(abap);
}

async function runCaught(contents: string) {
  const js = await run(contents);
  try {
    await new AsyncFunction("abap", js)(abap);
  } catch (error) {
    return error;
  }
  return undefined;
}

class IllegalForm extends Error {
  public async constructor_() {
    return this;
  }
}

class ProgramNotFound extends Error {
  public async constructor_() {
    return this;
  }
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

  it("PERFORM IN PROGRAM, registered form of another program", async () => {
    await runProgram("ztarget_perform.prog.abap", `
FORM hello.
  WRITE 'hello'.
ENDFORM.`);
    await runProgram("zfoobar_perform.prog.abap", `
DATA lv_prog TYPE c LENGTH 40.
PERFORM hello IN PROGRAM ztarget_perform.
lv_prog = 'ZTARGET_PERFORM'.
PERFORM hello IN PROGRAM (lv_prog).`);
    expect(abap.console.get()).to.equal("hellohello");
  });

  it("PERFORM IN PROGRAM, missing form of a loaded program, CX_SY_DYN_CALL_ILLEGAL_FORM", async () => {
    abap.Classes["CX_SY_DYN_CALL_ILLEGAL_FORM"] = IllegalForm;
    abap.Classes["CX_SY_PROGRAM_NOT_FOUND"] = ProgramNotFound;
    await runProgram("ztarget_perform.prog.abap", `
FORM hello.
  WRITE 'hello'.
ENDFORM.`);
    const caught = await runCaught(`PERFORM missing IN PROGRAM ztarget_perform.`);
    expect(caught).to.be.instanceof(IllegalForm);
  });

  it("PERFORM IN PROGRAM, initial dynamic name, CX_SY_PROGRAM_NOT_FOUND", async () => {
    abap.Classes["CX_SY_DYN_CALL_ILLEGAL_FORM"] = IllegalForm;
    abap.Classes["CX_SY_PROGRAM_NOT_FOUND"] = ProgramNotFound;
    const caught = await runCaught(`
DATA lv_prog TYPE c LENGTH 40.
PERFORM hello IN PROGRAM (lv_prog).
WRITE 'after'.`);
    expect(caught).to.be.instanceof(ProgramNotFound);
    expect(abap.console.get()).to.equal("");
  });

  it("PERFORM IN PROGRAM, unknown static name, CX_SY_PROGRAM_NOT_FOUND", async () => {
    abap.Classes["CX_SY_DYN_CALL_ILLEGAL_FORM"] = IllegalForm;
    abap.Classes["CX_SY_PROGRAM_NOT_FOUND"] = ProgramNotFound;
    const caught = await runCaught(`PERFORM hello IN PROGRAM zunknown.`);
    expect(caught).to.be.instanceof(ProgramNotFound);
  });

  it("PERFORM IN PROGRAM, exception class not loaded", async () => {
    const caught = await runCaught(`PERFORM hello IN PROGRAM zunknown.`);
    expect(caught).to.equal("CX_SY_PROGRAM_NOT_FOUND not found");
  });

});
