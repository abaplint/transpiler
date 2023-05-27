import {expect} from "chai";
import {ABAP, MemoryConsole} from "../../packages/runtime/src";
import {AsyncFunction, runFiles} from "../_utils";

let abap: ABAP;

async function run(contents: string) {
  return runFiles(abap, [{filename: "zfoobar.prog.abap", contents}]);
}

describe("Running Examples - string type", () => {

  beforeEach(async () => {
    abap = new ABAP({console: new MemoryConsole()});
  });

  it("set from field symbol", async () => {
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

  it("set from integer field symbol", async () => {
    const code = `
    DATA foo TYPE i VALUE 2.
    DATA str TYPE string.
    FIELD-SYMBOLS <fs> LIKE foo.
    ASSIGN foo TO <fs>.
    str = <fs>.
    str = '"' && str && '"'.
    WRITE str.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal(`"2 "`);
  });

  it("set from integer field symbol", async () => {
    const code = `
    DATA foo TYPE i.
    DATA str TYPE string.
    foo = -10.
    str = foo.
    WRITE str.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal(`10-`);
  });

});