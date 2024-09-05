import {expect} from "chai";
import {ABAP, MemoryConsole} from "../packages/runtime/src/";
import {AsyncFunction, runFiles} from "./_utils";
import {ITranspilerOptions} from "packages/transpiler/src";

let abap: ABAP;

async function run(contents: string) {
  const options: ITranspilerOptions = {
    keywords: [],
  };
  return runFiles(abap, [{filename: "zfoobar.prog.abap", contents}], options);
}

describe("Keywords", () => {

  beforeEach(async () => {
    abap = new ABAP({console: new MemoryConsole()});
  });

  it("class", async () => {
    const code = `
DATA class TYPE x.
class = 'AA'.
WRITE class.`;

    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("AA");
  });

});