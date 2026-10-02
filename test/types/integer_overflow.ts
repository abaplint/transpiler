import {expect} from "chai";
import {ABAP, MemoryConsole} from "../../packages/runtime/src/";
import {AsyncFunction, runFiles} from "../_utils";

let abap: ABAP;

async function run(contents: string) {
  return runFiles(abap, [{filename: "zfoobar.prog.abap", contents}]);
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
  lv_min = -2147483647.
  lv_min = lv_min - 1.`;

// What a SAP kernel answers for TYPE i at and beyond its range, measured with
// an ABAP Unit probe on a SAP system: a case name, the statements, and either
// the value WRITE gives for lv_i (or the variable named) or the class of the
// exception raised
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
];

describe("Running Examples - Integer overflow", () => {

  beforeEach(async () => {
    abap = new ABAP({console: new MemoryConsole()});
  });

  for (const c of cases) {
    it(c.name + ": " + c.expected, async () => {
      const code = DECLARATIONS + `
  ` + c.code + `
  WRITE ` + (c.write ?? "lv_i") + `.`;
      const js = await run(code);
      const f = new AsyncFunction("abap", js);
      if (c.expected.startsWith("CX_")) {
        let raised = "";
        try {
          await f(abap);
        } catch (e) {
          raised = e.toString();
        }
        expect(raised, "WRITE gave " + abap.console.get()).to.contain(c.expected);
      } else {
        await f(abap);
        expect(abap.console.get().trim()).to.equal(c.expected);
      }
    });
  }

});
