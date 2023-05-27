import {expect} from "chai";
import {ABAP, MemoryConsole} from "../../packages/runtime/src";
import {AsyncFunction, runFiles} from "../_utils";

let abap: ABAP;

async function run(contents: string) {
  return runFiles(abap, [{filename: "zfoobar.prog.abap", contents}]);
}

describe("Running statements - SET BIT", () => {

  beforeEach(async () => {
    abap = new ABAP({console: new MemoryConsole()});
  });

  it("SET BIT", async () => {
    const code = `
    DATA hex TYPE x LENGTH 1.
    DO 8 TIMES.
      IF sy-index > 4.
        CLEAR hex.
      ENDIF.
      SET BIT sy-index OF hex.
      WRITE / hex.
    ENDDO.

    DATA xstr TYPE xstring.
    xstr = 'F2420FA000'.
    SET BIT 30 OF xstr.
    SET BIT 25 OF xstr TO 0.
    SET BIT 35 OF xstr TO 1.
    WRITE / xstr.

    xstr = '03FF'.
    SET BIT 9 OF xstr TO 0.
    WRITE / xstr.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("80\nC0\nE0\nF0\n08\n04\n02\n01\nF2420F2420\n037F");
  });

  it("SET BIT, from offset, char", async () => {
    const code = `
  DATA rv_byte8 TYPE x LENGTH 8.
  DATA lv_offset TYPE i.
  DATA lv_output TYPE string.
  lv_output = '0000000'.
  lv_offset = 2.
  SET BIT 2 OF rv_byte8 TO lv_output+lv_offset(1).
  WRITE rv_byte8.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("0000000000000000");
  });

});