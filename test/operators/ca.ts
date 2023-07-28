import {expect} from "chai";
import {ABAP, MemoryConsole} from "../../packages/runtime/src";
import {AsyncFunction, runFiles} from "../_utils";

let abap: ABAP;

async function run(contents: string) {
  return runFiles(abap, [{filename: "zfoobar.prog.abap", contents}]);
}

describe("Running operators - CA", () => {

  beforeEach(async () => {
    abap = new ABAP({console: new MemoryConsole()});
  });

  it("CA", async () => {
    const code = `
      DATA bar TYPE abap_bool.
      bar = boolc( 'foo' CA 'a' ).
      ASSERT bar = abap_false.
      bar = boolc( 'foo' CA 'abc' ).
      ASSERT bar = abap_false.
      bar = boolc( 'foo' CA 'fo' ).
      ASSERT bar = abap_true.
      bar = boolc( 'foo' CA 'o' ).
      ASSERT bar = abap_true.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
  });

  it("CA, empty left -> space", async () => {
    const code = `
      ASSERT NOT '' CA 'AB'.
      ASSERT '' CA 'A B'.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
  });

  it("set fdpos, CA", async () => {
    const code = `
IF '12C' CA sy-abcde.
  WRITE / sy-fdpos.
ENDIF.
IF NOT '123' CA sy-abcde.
  WRITE / sy-fdpos.
ENDIF.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal(`2\n3`);
  });

  it("CA, structured fs", async () => {
    const code = `
DATA: BEGIN OF bar,
        field1 TYPE c LENGTH 1,
        field2 TYPE c LENGTH 1,
        field3 TYPE c LENGTH 1,
      END OF bar.
FIELD-SYMBOLS <fs> TYPE any.
bar-field2 = abap_true.
ASSIGN bar TO <fs>.
ASSERT <fs> CA abap_true.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
  });

});