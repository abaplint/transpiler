// import {expect} from "chai";
import {ABAP} from "../../packages/runtime/src";
import {AsyncFunction, runFiles} from "../_utils";

let abap: ABAP;

async function run(contents: string) {
  return runFiles(abap, [{filename: "zfoobar.prog.abap", contents}]);
}

describe("Builtin functions - to_mixed", () => {

  beforeEach(async () => {
    abap = new ABAP();
  });

  it("test1", async () => {
    const code = `
    DATA m TYPE string.
    m = to_mixed( val = |Hello World| ).
    ASSERT m = 'Hello world'.
    `;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
  });

});
