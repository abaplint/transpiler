import {expect} from "chai";
import {ABAP, MemoryConsole} from "../../packages/runtime/src/";
import {AsyncFunction, runFiles} from "../_utils";

let abap: ABAP;

const EXCEPTIONS = ["CX_SY_ARITHMETIC_OVERFLOW", "CX_SY_CONVERSION_OVERFLOW", "CX_SY_ZERODIVIDE", "CX_SY_RANGE_OUT_OF_BOUNDS"];

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
  DATA lv_p1 TYPE p LENGTH 16 DECIMALS 1.
  DATA lv_f TYPE f.
  DATA lv_c TYPE c LENGTH 12.
  DATA lv_s TYPE string.
  DATA lv_8 TYPE int8.
  DATA lt_tab TYPE STANDARD TABLE OF i WITH DEFAULT KEY.
  lv_min = -2147483647.
  lv_min = lv_min - 1.`;

// What a SAP kernel answers for TYPE i at and beyond its range, measured with
// an ABAP Unit probe on a SAP system: a case name, the statements, and either
// the value WRITE gives for lv_i (or the variable named) or the class of the
// exception raised. Cases marked UNMEASURED were not on the probe, they pin
// what this runtime does by the same rule
type Case = {name: string, code: string, write?: string, expected: string};

const cases: Case[] = [
  {name: "min", code: ``, write: "lv_min", expected: "-2147483648"},
  {name: "max + 1", code: `lv_i = lv_max + 1.`, expected: "CX_SY_ARITHMETIC_OVERFLOW"},
  {name: "min - 1", code: `lv_i = lv_min - 1.`, expected: "CX_SY_ARITHMETIC_OVERFLOW"},
  {name: "ADD", code: `lv_i = lv_max.
  ADD 1 TO lv_i.`, expected: "CX_SY_ARITHMETIC_OVERFLOW"},
  {name: "max * 2", code: `lv_i = lv_max * lv_two.`, expected: "CX_SY_ARITHMETIC_OVERFLOW"},
  {name: "MULTIPLY", code: `lv_i = lv_max.
  MULTIPLY lv_i BY 2.`, expected: "CX_SY_ARITHMETIC_OVERFLOW"},
  {name: "min * -1", code: `lv_i = lv_min * lv_m1.`, expected: "CX_SY_ARITHMETIC_OVERFLOW"},
  {name: "min DIV -1", code: `lv_i = lv_min DIV lv_m1.`, expected: "CX_SY_ARITHMETIC_OVERFLOW"},
  {name: "min MOD -1", code: `lv_i = lv_min MOD lv_m1.`, expected: "0"},
  {name: "abs( min )", code: `lv_i = abs( lv_min ).`, expected: "CX_SY_ARITHMETIC_OVERFLOW"},
  {name: "0 - min", code: `lv_i = 0 - lv_min.`, expected: "CX_SY_ARITHMETIC_OVERFLOW"},
  {name: "p = max + 1, the calculation type is p", code: `lv_p = lv_max + 1.`, write: "lv_p", expected: "2147483648"},
  {name: "i = max + p", code: `lv_p = 1.
  lv_i = lv_max + lv_p.`, expected: "CX_SY_ARITHMETIC_OVERFLOW"},
  {name: "int8 = max + 1, the calculation type is int8", code: `lv_8 = lv_max + 1.`, write: "lv_8", expected: "2147483648"},
  {name: "i = int8 3e9", code: `lv_8 = 3000000000.
  lv_i = lv_8.`, expected: "CX_SY_CONVERSION_OVERFLOW"},
  {name: "i = f 3e9", code: `lv_f = '3E9'.
  lv_i = lv_f.`, expected: "CX_SY_CONVERSION_OVERFLOW"},
  {name: "i = f -3e9", code: `lv_f = '-3E9'.
  lv_i = lv_f.`, expected: "CX_SY_CONVERSION_OVERFLOW"},
  {name: "i = f max.5", code: `lv_f = '2147483647.5'.
  lv_i = lv_f.`, expected: "CX_SY_CONVERSION_OVERFLOW"},
  {name: "i = f max.4", code: `lv_f = '2147483647.4'.
  lv_i = lv_f.`, expected: "2147483647"},
  {name: "i = f min.5", code: `lv_f = '-2147483648.5'.
  lv_i = lv_f.`, expected: "CX_SY_CONVERSION_OVERFLOW"},
  {name: "i = p 3e9", code: `lv_p = 3000000000.
  lv_i = lv_p.`, expected: "CX_SY_CONVERSION_OVERFLOW"},
  {name: "i = p max.5", code: `lv_p1 = '2147483647.5'.
  lv_i = lv_p1.`, expected: "CX_SY_CONVERSION_OVERFLOW"},
  {name: "i = p min.4", code: `lv_p1 = '-2147483648.4'.
  lv_i = lv_p1.`, expected: "-2147483648"},
  {name: "i = p min.5", code: `lv_p1 = '-2147483648.5'.
  lv_i = lv_p1.`, expected: "CX_SY_CONVERSION_OVERFLOW"},
  {name: "i = c 3e9", code: `lv_c = '3000000000'.
  lv_i = lv_c.`, expected: "CX_SY_CONVERSION_OVERFLOW"},
  {name: "i = string max + 1", code: `lv_s = |2147483648|.
  lv_i = lv_s.`, expected: "CX_SY_CONVERSION_OVERFLOW"},
  {name: "7 / 2", code: `lv_i = 7 / 2.`, expected: "4"},
  {name: "-7 / 2", code: `lv_i = -7 / 2.`, expected: "-4"},
  {name: "5 / 2", code: `lv_i = 5 / 2.`, expected: "3"},
  {name: "-5 / 2", code: `lv_i = -5 / 2.`, expected: "-3"},
  {name: "7 DIV -2", code: `lv_i = 7 DIV -2.`, expected: "-3"},
  {name: "7 MOD -2", code: `lv_i = 7 MOD -2.`, expected: "1"},
  {name: "-7 DIV 2", code: `lv_i = -7 DIV 2.`, expected: "-4"},
  {name: "-7 MOD 2", code: `lv_i = -7 MOD 2.`, expected: "1"},
  {name: "i / 0", code: `lv_i = lv_max.
  lv_i = lv_i / 0.`, expected: "CX_SY_ZERODIVIDE"},
  {name: "0 / 0", code: `lv_i = 0.
  lv_i = lv_i / 0.`, expected: "0"},
  // the same rules, as further probes on the same system answered them
  {name: "i * i past max", code: `lv_i = 479001600.
  lv_i = lv_i * 13.`, expected: "CX_SY_ARITHMETIC_OVERFLOW"},
  {name: "max - 1 + 1 fits", code: `lv_i = 2147483646.
  lv_i = lv_i + 1.`, expected: "2147483647"},
  {name: "i * i up to max fits", code: `lv_i = 39916800.
  lv_i = lv_i * 12.`, expected: "479001600"},
  {name: "string template of an i expression past max", code: `lv_i = 20713.
  lv_s = |{ ( lv_i * 86400 + 0 ) * 1000 }|.`, expected: "CX_SY_ARITHMETIC_OVERFLOW"},
  // positions of type i: the result of i arithmetic is checked there too
  {name: "template, i division past max", code: `lv_s = |{ ( lv_max + 1 ) / 1 }|.`, expected: "CX_SY_ARITHMETIC_OVERFLOW"},
  {name: "UNMEASURED: READ TABLE INDEX past max", code: `APPEND 1 TO lt_tab.
  READ TABLE lt_tab INDEX lv_max + 1 INTO lv_i.`, expected: "CX_SY_ARITHMETIC_OVERFLOW"},
  {name: "UNMEASURED: LOOP FROM past max", code: `APPEND 1 TO lt_tab.
  LOOP AT lt_tab INTO lv_i FROM lv_max + 1.
  ENDLOOP.`, expected: "CX_SY_ARITHMETIC_OVERFLOW"},
  {name: "UNMEASURED: substring( ) offset past max", code: `lv_s = |abc|.
  lv_s = substring( val = lv_s off = lv_max + 1 len = 1 ).`, expected: "CX_SY_ARITHMETIC_OVERFLOW"},
  {name: "UNMEASURED: sy-tabix plus max", code: `APPEND 1 TO lt_tab.
  LOOP AT lt_tab INTO lv_i.
    lv_i = sy-tabix + lv_max.
  ENDLOOP.`, expected: "CX_SY_ARITHMETIC_OVERFLOW"},
  {name: "UNMEASURED: DO with a p count past max", code: `lv_p = 3000000000.
  DO lv_p TIMES.
    EXIT.
  ENDDO.`, expected: "CX_SY_CONVERSION_OVERFLOW"},
  {name: "DO with an i count of max", code: `DO lv_max TIMES.
    lv_i = sy-index.
    EXIT.
  ENDDO.`, expected: "1"},
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
    CATCH cx_sy_conversion_overflow.
      WRITE 'CX_SY_CONVERSION_OVERFLOW'.
    CATCH cx_sy_zerodivide.
      WRITE 'CX_SY_ZERODIVIDE'.
    CATCH cx_sy_range_out_of_bounds.
      WRITE 'CX_SY_RANGE_OUT_OF_BOUNDS'.
  ENDTRY.`;
      const js = await run(code);
      const f = new AsyncFunction("abap", js);
      await f(abap);
      expect(abap.console.get().trim()).to.equal(c.expected);
    });
  }

});
