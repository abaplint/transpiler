import {expect} from "chai";
import {ABAP, MemoryConsole} from "../../packages/runtime/src";
import {runFiles} from "../_utils";

let abap: ABAP;

async function run(contents: string) {
  return runFiles(abap, [{filename: "zfoobar.prog.abap", contents}]);
}

describe("Running statements - LOOP AT SCREEN", () => {

  beforeEach(async () => {
    abap = new ABAP({console: new MemoryConsole()});
  });

  it("Basic", async () => {
    const code = `
LOOP AT SCREEN.
ENDLOOP.`;
    const js = await run(code);
    expect(js).to.include("{");
    expect(js).to.include("}");
  });

});