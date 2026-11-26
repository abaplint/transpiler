import {expect} from "chai";
import {ABAP, MemoryConsole} from "../../packages/runtime/src/";
import {AsyncFunction, runFiles} from "../_utils";

let abap: ABAP;

const EXCEPTIONS = ["CX_SY_ARITHMETIC_OVERFLOW"];

const cxroot = `
CLASS cx_root DEFINITION PUBLIC.
ENDCLASS.
CLASS cx_root IMPLEMENTATION.
ENDCLASS.`;

const cx = (name: string) => `
CLASS ${name} DEFINITION PUBLIC INHERITING FROM cx_root.
ENDCLASS.
CLASS ${name} IMPLEMENTATION.
ENDCLASS.`;

// the program first: runFiles evaluates the first object only, so the exception
// classes are in the registry for the syntax check and stand in as plain
// JavaScript classes at run time
async function run(contents: string) {
  const files = [{filename: "zfoobar.prog.abap", contents}, {filename: "cx_root.clas.abap", contents: cxroot}];
  for (const name of EXCEPTIONS) {
    files.push({filename: name.toLowerCase() + ".clas.abap", contents: cx(name.toLowerCase())});
  }
  const js = await runFiles(abap, files);
  abap.Classes["CX_ROOT"] = class CxRoot {};
  for (const name of EXCEPTIONS) {
    abap.Classes[name] = class extends abap.Classes["CX_ROOT"] {};
  }
  return js;
}

const DECLARATIONS = `
  DATA lv_max TYPE i VALUE 2147483647.
  DATA lv_min TYPE i.
  DATA lv_i TYPE i.
  DATA lv_two TYPE i VALUE 2.
  DATA lv_m1 TYPE i VALUE -1.
  DATA lv_p TYPE p LENGTH 16 DECIMALS 0.
  DATA lv_8 TYPE int8.
  lv_min = -2147483647.
  lv_min = lv_min - 1.`;

// What a SAP kernel answers for "+", "-" and "*" in calculation type i at and
// beyond the range of i, measured with an ABAP Unit probe on a SAP system: a
// case name, the statements, and either the value WRITE gives for lv_i (or the
// variable named) or the class of the exception raised. Cases named "not on
// the probe" were not measured, they say where their expectation comes from
type Case = {name: string, code: string, write?: string, expected: string};

const cases: Case[] = [
  {name: "min", code: ``, write: "lv_min", expected: "-2147483648"},
  {name: "max + 1", code: `lv_i = lv_max + 1.`, expected: "CX_SY_ARITHMETIC_OVERFLOW"},
  {name: "min - 1", code: `lv_i = lv_min - 1.`, expected: "CX_SY_ARITHMETIC_OVERFLOW"},
  {name: "max * 2", code: `lv_i = lv_max * lv_two.`, expected: "CX_SY_ARITHMETIC_OVERFLOW"},
  {name: "min * -1", code: `lv_i = lv_min * lv_m1.`, expected: "CX_SY_ARITHMETIC_OVERFLOW"},
  {name: "0 - min", code: `lv_i = 0 - lv_min.`, expected: "CX_SY_ARITHMETIC_OVERFLOW"},
  {name: "i * i past max", code: `lv_i = 479001600.
  lv_i = lv_i * 13.`, expected: "CX_SY_ARITHMETIC_OVERFLOW"},
  {name: "max - 1 + 1 fits", code: `lv_i = 2147483646.
  lv_i = lv_i + 1.`, expected: "2147483647"},
  {name: "i * i up to max fits", code: `lv_i = 39916800.
  lv_i = lv_i * 12.`, expected: "479001600"},
  {name: "not on the probe, unchanged from main: min + max fits", code: `lv_i = lv_min + lv_max.`, expected: "-1"},
  // an interim result is in calculation type i as well (ABAP keyword documentation,
  // calculation type, "calculation types i and int8"); these were not on the probe
  {name: "not on the probe: max + 1 - 1, the interim result", code: `lv_i = lv_max + 1 - 1.`, expected: "CX_SY_ARITHMETIC_OVERFLOW"},
  {name: "not on the probe: ( max + 1 ) - 1, the interim result", code: `lv_i = ( lv_max + 1 ) - 1.`, expected: "CX_SY_ARITHMETIC_OVERFLOW"},
  // a sign in front of an operand is a multiplication by -1 (ABAP keyword documentation,
  // arithmetic operators); not on the probe
  {name: "not on the probe: - min - 1, the sign is a calculation", code: `lv_i = - lv_min - 1.`, expected: "CX_SY_ARITHMETIC_OVERFLOW"},
  // target and operands together decide the calculation type: nothing raises here
  {name: "p = max + 1, the calculation type is p", code: `lv_p = lv_max + 1.`, write: "lv_p", expected: "2147483648"},
  {name: "int8 = max + 1, the calculation type is int8", code: `lv_8 = lv_max + 1.`, write: "lv_8", expected: "2147483648"},
  {name: "not on the probe, unchanged from main: int8 = max * max, the calculation type is int8", code: `lv_8 = lv_max * lv_max.`, write: "lv_8", expected: "4611686014132420609"},
  // division is not part of this, pinned as it is
  {name: "7 / 2", code: `lv_i = 7 / 2.`, expected: "4"},
  {name: "-7 / 2", code: `lv_i = -7 / 2.`, expected: "-4"},
  {name: "7 DIV -2", code: `lv_i = 7 DIV -2.`, expected: "-3"},
  {name: "-7 MOD 2", code: `lv_i = -7 MOD 2.`, expected: "1"},
  // not on the probe: the same calculation, the operand or the target reached another way
  {name: "a field symbol typed i as operand, not on the probe",
    code: `FIELD-SYMBOLS <lv_fs> TYPE i.
  ASSIGN lv_max TO <lv_fs>.
  lv_i = <lv_fs> + 1.`, expected: "CX_SY_ARITHMETIC_OVERFLOW"},
  {name: "a structure component as target, not on the probe",
    code: `DATA: BEGIN OF ls_data, comp TYPE i, END OF ls_data.
  ls_data-comp = lv_max + 1.`, expected: "CX_SY_ARITHMETIC_OVERFLOW"},
];

describe("Running Examples - Integer overflow", () => {

  beforeEach(async () => {
    abap = new ABAP({console: new MemoryConsole()});
  });

  for (const c of cases) {
    it(c.name + ": " + c.expected, async () => {
      const code = DECLARATIONS + `
  TRY.
  ` + c.code + `
      WRITE ` + (c.write ?? "lv_i") + `.
    CATCH cx_sy_arithmetic_overflow.
      WRITE 'CX_SY_ARITHMETIC_OVERFLOW'.
  ENDTRY.`;
      const js = await run(code);
      const f = new AsyncFunction("abap", js);
      await f(abap);
      expect(abap.console.get().trim()).to.equal(c.expected);
    });
  }

  it("an unassigned field symbol still fails", async () => {
    const code = `
  DATA lv_i TYPE i.
  FIELD-SYMBOLS <lv_i> TYPE i.
  lv_i = <lv_i> + 1.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    try {
      await f(abap);
      expect.fail();
    } catch (e) {
      expect(e.toString()).to.contain("GETWA_NOT_ASSIGNED");
    }
  });

});
