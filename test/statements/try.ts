import {ABAP, MemoryConsole} from "../../packages/runtime/src";
import {expect} from "chai";
import {AsyncFunction, runFiles} from "../_utils";
import {UnknownTypesEnum} from "../../packages/transpiler/src/types";

let abap: ABAP;

async function run(contents: string) {
  return runFiles(abap, [{filename: "zfoobar.prog.abap", contents}], {unknownTypes: UnknownTypesEnum.runtimeError});
}

describe("Running statements - TRY", () => {

  beforeEach(async () => {
    abap = new ABAP({console: new MemoryConsole()});
  });

  it("TRY without CATCH", async () => {
    const code = `
      TRY.
        WRITE 'hello'.
      ENDTRY.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
  });

  it("TRY, non existing class", async () => {
    const code = `
      TRY.
        WRITE '@KERNEL throw "hello";'.
      CATCH cx_not_existing.
      ENDTRY.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    try {
      await f(abap);
    } catch (e) {
      expect(e + "").to.include("hello");
    }
  });

});