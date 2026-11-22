import {expect} from "chai";
import {ABAP, MemoryConsole} from "../../packages/runtime/src";
import {AsyncFunction, runFiles} from "../_utils";

let abap: ABAP;

async function run(contents: string) {
  return runFiles(abap, [{filename: "zfoobar_local.prog.abap", contents}], {skipVersionCheck: true});
}

describe("Running statements - LOCAL", () => {

  beforeEach(async () => {
    abap = new ABAP({console: new MemoryConsole()});
  });

  it("puts the field back when the FORM ends", async () => {
    const code = `
DATA value TYPE c LENGTH 3.
DATA count TYPE i.

FORM change.
  LOCAL value.
  LOCAL count.
  value = 'in'.
  count = 5.
  WRITE / |{ value } { count }|.
  IF count = 5.
    RETURN.
  ENDIF.
  value = 'no'.
ENDFORM.

START-OF-SELECTION.
  value = 'out'.
  count = 1.
  PERFORM change.
  WRITE / |{ value } { count }|.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("in 5\nout 1");
  });

});
