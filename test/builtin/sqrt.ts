import {expect} from "chai";
import {ABAP} from "../../packages/runtime/src/index.js";
import {AsyncFunction, runFiles} from "../_utils.js";

let abap: ABAP;

async function run(contents: string) {
  return runFiles(abap, [{filename: "zfoobar.prog.abap", contents}]);
}

describe("Builtin functions - sqrt", () => {

  beforeEach(async () => {
    abap = new ABAP();
  });

  it("basic sqrt()", async () => {
    const code = "ASSERT sqrt( 4 ) = 2.";
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
  });

  it("basic sqrt(), round to int", async () => {
    const code = `
DATA i TYPE i.
i = sqrt( 50 ).
WRITE / i.
i = sqrt( 58 ).
WRITE / i.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal( `7\n8` );
  });

});
