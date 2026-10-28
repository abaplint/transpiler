import {expect} from "chai";
import {ABAP, MemoryConsole} from "../../packages/runtime/src/";
import {AsyncFunction, runFiles} from "../_utils";

let abap: ABAP;

const cxroot = `
CLASS cx_root DEFINITION PUBLIC.
ENDCLASS.
CLASS cx_root IMPLEMENTATION.
ENDCLASS.`;

const cxconv = `
CLASS cx_sy_conversion_no_number DEFINITION PUBLIC INHERITING FROM cx_root.
ENDCLASS.
CLASS cx_sy_conversion_no_number IMPLEMENTATION.
ENDCLASS.`;

async function run(contents: string) {
  // the program first: runFiles evaluates the first object only, so the
  // exception classes are in the registry for the syntax check and stand in
  // as plain JavaScript classes at run time
  const js = await runFiles(abap, [
    {filename: "zfoobar.prog.abap", contents},
    {filename: "cx_root.clas.abap", contents: cxroot},
    {filename: "cx_sy_conversion_no_number.clas.abap", contents: cxconv}]);
  abap.Classes["CX_ROOT"] = class CxRoot {};
  abap.Classes["CX_SY_CONVERSION_NO_NUMBER"] = class CxSyConversionNoNumber extends abap.Classes["CX_ROOT"] {};
  const f = new AsyncFunction("abap", js);
  await f(abap);
}

// A conversion that raises CX_SY_CONVERSION_NO_NUMBER leaves its target initial on a system:
// the handler sees the target cleared, not the value it held before
describe("Running Examples - a failed conversion clears its target", () => {

  beforeEach(async () => {
    abap = new ABAP({console: new MemoryConsole()});
  });

  for (const type of ["i", "int8", "p LENGTH 8 DECIMALS 2", "f"]) {
    it(`TYPE ${type}, from a string`, async () => {
      await run(`
DATA target TYPE ${type}.
DATA text TYPE string VALUE 'seven'.
target = 5.
TRY.
    target = text.
  CATCH cx_sy_conversion_no_number.
    WRITE 'caught'.
ENDTRY.
IF target IS INITIAL.
  WRITE / 'initial'.
ENDIF.`);
      expect(abap.console.get()).to.equal("caught\ninitial");
    });
  }

  it("TYPE i, from a character field", async () => {
    await run(`
DATA target TYPE i.
DATA text TYPE c LENGTH 5 VALUE 'abc'.
target = 5.
TRY.
    target = text.
  CATCH cx_sy_conversion_no_number.
ENDTRY.
WRITE target.`);
    expect(abap.console.get()).to.equal("0");
  });

  it("a conversion that succeeds still sets the value", async () => {
    await run(`
DATA target TYPE i.
DATA text TYPE string VALUE '12'.
target = 5.
target = text.
WRITE target.`);
    expect(abap.console.get()).to.equal("12");
  });

});
