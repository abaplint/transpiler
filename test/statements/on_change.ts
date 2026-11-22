import {expect} from "chai";
import {ABAP, MemoryConsole} from "../../packages/runtime/src";
import {AsyncFunction, runFiles} from "../_utils";

let abap: ABAP;

async function run(contents: string) {
  return runFiles(abap, [{filename: "zfoobar_on_change.prog.abap", contents}], {skipVersionCheck: true});
}

describe("Running statements - ON CHANGE OF", () => {

  beforeEach(async () => {
    abap = new ABAP({console: new MemoryConsole()});
  });

  it("remembers the last value per statement, also across FORM calls", async () => {
    const code = `
DATA value TYPE c LENGTH 2.

FORM check USING iv TYPE c.
  value = iv.
  ON CHANGE OF value.
    WRITE / |new { value }|.
  ELSE.
    WRITE / |same { value }|.
  ENDON.
ENDFORM.

START-OF-SELECTION.
  PERFORM check USING ' '.
  PERFORM check USING 'A'.
  PERFORM check USING 'A'.
  PERFORM check USING 'B'.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("same \nnew A\nsame A\nnew B");
  });

  it("OR, a change in any field", async () => {
    const code = `
DATA a TYPE i.
DATA b TYPE i.
DO 3 TIMES.
  IF sy-index = 3.
    b = 1.
  ENDIF.
  ON CHANGE OF a OR b.
    WRITE / sy-index.
  ENDON.
ENDDO.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("3");
  });

});
