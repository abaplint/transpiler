import {expect} from "chai";
import {ABAP, MemoryConsole} from "../../packages/runtime/src";
import {AsyncFunction, runFiles} from "../_utils";

let abap: ABAP;

async function run(contents: string) {
  return runFiles(abap, [{filename: "zfoobar_line_index.prog.abap", contents}]);
}

describe("Builtin functions - line_index", () => {

  beforeEach(async () => {
    abap = new ABAP({console: new MemoryConsole()});
  });

  it("positive", async () => {
    const code = `
DATA tab TYPE STANDARD TABLE OF i WITH EMPTY KEY.
DATA int TYPE i.
INSERT VALUE #( ) INTO TABLE tab.
int = line_index( tab[ table_line = 0 ] ).
WRITE / int.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal(`1`);
  });

  it("negative", async () => {
    const code = `
DATA tab TYPE STANDARD TABLE OF i WITH EMPTY KEY.
DATA int TYPE i.
int = line_index( tab[ table_line = 0 ] ).
WRITE / int.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal(`0`);
  });

});
