import {expect} from "chai";
import {ABAP, MemoryConsole} from "../../packages/runtime/src";
import {AsyncFunction, runFiles} from "../_utils";

let abap: ABAP;

async function run(contents: string) {
  return runFiles(abap, [{filename: "zfoobar.prog.abap", contents}]);
}

describe("Running operators - CS", () => {

  beforeEach(async () => {
    abap = new ABAP({console: new MemoryConsole()});
  });

  it("CS should set sy-fdpos, negative", async () => {
    const code = `
    IF |abcd| CS |werrwewerwerwer|.
    ENDIF.
    assert sy-fdpos = 4.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
  });

  it("CS, basic", async () => {
    const code = `ASSERT 'test' CS 't'.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
  });

  it("CS, case insensitive", async () => {
    const code = `ASSERT 'test' CS 'T'.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
  });

  it("CS should set sy-fdpos", async () => {
    const code = `
    IF |ffffabc| CS |abc|.
    ELSE.
      ASSERT 'nah' = 1.
    ENDIF.
    ASSERT sy-fdpos = 4.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
  });

  it("CS whitespace, character type", async () => {
    const code = `
DATA bool    TYPE abap_bool.
DATA absolute_name type c length 200.

absolute_name = \`TYPE-POOL=ABAPTYPE=ABAP_BOOL  \`.

IF \`TYPE-POOL=ABAPTYPE=ABAP_BOOLTYPE=BOOLEANTYPE=BOOLE_DTYPE=XFELD\` CS absolute_name.
  write 'yes'.
else.
  write 'no'.
ENDIF.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal(`yes`);
  });

});