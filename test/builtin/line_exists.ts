import {expect} from "chai";
import {ABAP, MemoryConsole} from "../../packages/runtime/src";
import {AsyncFunction, runFiles} from "../_utils";

let abap: ABAP;

async function run(contents: string) {
  return runFiles(abap, [{filename: "zfoobar_line_exists.prog.abap", contents}]);
}

describe.skip("Builtin functions - line_exists", () => {

  beforeEach(async () => {
    abap = new ABAP({console: new MemoryConsole()});
  });

  it("positive", async () => {
    const code = `
DATA tab TYPE STANDARD TABLE OF i WITH EMPTY KEY.
INSERT VALUE #( ) INTO TABLE tab.
IF line_exists( tab[ 1 ] ).
  WRITE / 'yes'.
ELSE.
  WRITE / 'no'.
ENDIF.
      `;
    const js = await run(code);
    console.dir(js);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal(`yes`);
  });

  it("negative", async () => {
    const code = `
DATA tab TYPE STANDARD TABLE OF i WITH EMPTY KEY.
IF line_exists( tab[ 1 ] ).
  WRITE / 'yes'.
ELSE.
  WRITE / 'no'.
ENDIF.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal(`no`);
  });

});
