import {expect} from "chai";
import {ABAP} from "../../packages/runtime/src";
import {AsyncFunction, runFiles} from "../_utils";

let abap: ABAP;

async function run(contents: string) {
  return runFiles(abap, [{filename: "zfoobar.prog.abap", contents}]);
}

describe("Running Examples - string type", () => {

  beforeEach(async () => {
    abap = new ABAP();
  });

  it.skip("set from field symbol", async () => {
    const code = `
    DATA char3 TYPE c LENGTH 3.
    DATA str TYPE string.
    FIELD-SYMBOLS <fs> LIKE char3.
    ASSIGN char3 TO <fs>.
    str = <fs>.
    str = '"' && str && '"'.
    WRITE str.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal(`""`);
  });

});