import {expect} from "chai";
import {ABAP, MemoryConsole} from "../../packages/runtime/src/";
import {AsyncFunction, runFiles} from "../_utils";

let abap: ABAP;

const exceptions = ["CX_SY_ARITHMETIC_OVERFLOW", "CX_SY_CONVERSION_OVERFLOW"];

function exceptionClass(name: string, superclass?: string): string {
  return `
CLASS ${name} DEFINITION PUBLIC${superclass ? " INHERITING FROM " + superclass : ""}.
ENDCLASS.
CLASS ${name} IMPLEMENTATION.
ENDCLASS.`;
}

async function output(contents: string): Promise<string> {
  // the program first: runFiles evaluates the first object only, so the exception classes
  // are in the registry for the syntax check and stand in as plain JavaScript classes at run time
  const js = await runFiles(abap, [
    {filename: "zfoobar.prog.abap", contents},
    {filename: "cx_root.clas.abap", contents: exceptionClass("cx_root")},
    ...exceptions.map(e => ({filename: e.toLowerCase() + ".clas.abap", contents: exceptionClass(e.toLowerCase(), "cx_root")}))]);
  abap.Classes["CX_ROOT"] = class CxRoot {};
  for (const e of exceptions) {
    abap.Classes[e] = class extends abap.Classes["CX_ROOT"] {};
  }
  const f = new AsyncFunction("abap", js);
  await f(abap);
  return abap.console.get();
}

// each statement in its own TRY, the output is the value or the name of the exception
function tries(declarations: string, statements: string[], exception: string): string {
  let code = declarations + "\n";
  for (const s of statements) {
    code += `TRY.
    ${s}
  CATCH ${exception}.
    WRITE / '${exception.toUpperCase()}'.
ENDTRY.
`;
  }
  return code;
}

describe("Running Examples - Integer overflow", () => {

  beforeEach(async () => {
    abap = new ABAP({console: new MemoryConsole()});
  });

  describe("calculation type i, CX_SY_ARITHMETIC_OVERFLOW", () => {
    const declarations = `
DATA lv_max TYPE i VALUE 2147483647.
DATA lv_min TYPE i.
DATA lv_a TYPE i VALUE 65536.
DATA lv_one TYPE i VALUE 1.
DATA lv_r TYPE i.
lv_min = -2147483647 - 1.`;

    const cases: [string, string][] = [
      ["lv_r = lv_max + 1.", "max + 1"],
      ["lv_r = lv_min - 1.", "min - 1"],
      ["lv_r = lv_a * lv_a.", "65536 * 65536"],
      ["lv_r = lv_max + lv_one - lv_one.", "intermediate max + 1"],
      ["lv_r = ( lv_max + lv_one ) - lv_one.", "intermediate in parentheses"],
      ["lv_r = lv_min * -1.", "min * -1"],
      ["lv_r = lv_min DIV -1.", "min DIV -1"],
      ["lv_r = lv_min / -1.", "min / -1"],
      ["lv_r = abs( lv_min ).", "abs( min )"],
      ["lv_r = lv_max. ADD 1 TO lv_r.", "ADD"],
      ["lv_r = lv_min. SUBTRACT 1 FROM lv_r.", "SUBTRACT"],
      ["lv_r = lv_a. MULTIPLY lv_r BY lv_a.", "MULTIPLY"],
      ["lv_r = lv_max. lv_r += 1.", "+="],
      ["lv_r = lv_min. lv_r -= 1.", "-="],
      ["lv_r = lv_a. lv_r *= lv_a.", "*="],
    ];
    for (const [statement, name] of cases) {
      it(name, async () => {
        const out = await output(tries(declarations, [statement + " WRITE / lv_r."], "cx_sy_arithmetic_overflow"));
        expect(out).to.equal("CX_SY_ARITHMETIC_OVERFLOW");
      });
    }

    it("the target keeps its value", async () => {
      const out = await output(tries(declarations + "\nlv_r = 5.", ["lv_r = lv_max + 1."], "cx_sy_arithmetic_overflow") + "WRITE / lv_r.");
      expect(out).to.equal("CX_SY_ARITHMETIC_OVERFLOW\n5");
    });

    it("in range, min MOD -1, the edges", async () => {
      const out = await output(declarations + `
lv_r = lv_min MOD -1.
WRITE / lv_r.
lv_r = lv_max - 1 + 1.
WRITE / lv_r.
lv_r = lv_min + 1 - 1.
WRITE / lv_r.
lv_r = lv_max.
ADD 0 TO lv_r.
WRITE / lv_r.`);
      expect(out).to.equal("0\n2147483647\n-2147483648\n2147483647");
    });

    it("COLLECT", async () => {
      const out = await output(`
TYPES: BEGIN OF ty_row,
         key    TYPE c LENGTH 1,
         amount TYPE i,
       END OF ty_row.
DATA lt_rows TYPE STANDARD TABLE OF ty_row WITH DEFAULT KEY.
DATA ls_row TYPE ty_row.
ls_row-key = 'A'.
ls_row-amount = 2147483647.
COLLECT ls_row INTO lt_rows.
TRY.
    ls_row-amount = 1.
    COLLECT ls_row INTO lt_rows.
    READ TABLE lt_rows INTO ls_row INDEX 1.
    WRITE / ls_row-amount.
  CATCH cx_sy_arithmetic_overflow.
    WRITE / 'CX_SY_ARITHMETIC_OVERFLOW'.
ENDTRY.`);
      expect(out).to.equal("CX_SY_ARITHMETIC_OVERFLOW");
    });

    it("n operands, i target", async () => {
      const out = await output(tries(`
DATA lv_n3 TYPE n LENGTH 3 VALUE '999'.
DATA lv_i TYPE i.`, ["lv_i = lv_n3 * lv_n3 * lv_n3 * lv_n3. WRITE / lv_i."], "cx_sy_arithmetic_overflow"));
      expect(out).to.equal("CX_SY_ARITHMETIC_OVERFLOW");
    });
  });

  describe("other calculation types, no overflow in i", () => {
    it("p and int8 targets take the result as it is", async () => {
      const out = await output(`
DATA lv_max TYPE i VALUE 2147483647.
DATA lv_one TYPE i VALUE 1.
DATA lv_p TYPE p LENGTH 8.
DATA lv_i8 TYPE int8.
DATA lv_f TYPE f.
lv_p = lv_max + lv_one.
WRITE / lv_p.
lv_i8 = lv_max + lv_one.
WRITE / lv_i8.
lv_i8 = lv_max.
ADD 1 TO lv_i8.
WRITE / lv_i8.
lv_f = lv_max + lv_one.
WRITE / |{ lv_f }|.`);
      expect(out).to.equal("2147483648\n2147483648\n2147483648\n2147483648");
    });
  });

  describe("CX_SY_CONVERSION_OVERFLOW", () => {
    const declarations = `
DATA lv_r TYPE i.
DATA lv_a TYPE i VALUE 2.
DATA lv_f TYPE f.
DATA lv_i8 TYPE int8 VALUE 3000000000.
DATA lv_p TYPE p LENGTH 8 DECIMALS 2 VALUE '3000000000'.
DATA lv_n20 TYPE n LENGTH 20 VALUE '99999999999999999999'.
DATA lv_r8 TYPE int8.`;

    const cases: [string, string][] = [
      ["lv_f = '1E10'. lv_r = lv_f. WRITE / lv_r.", "f into i"],
      ["lv_r = lv_i8. WRITE / lv_r.", "int8 into i"],
      ["lv_r = lv_p. WRITE / lv_r.", "p into i"],
      ["lv_r = '3000000000'. WRITE / lv_r.", "text into i"],
      ["lv_r = lv_a ** 31. WRITE / lv_r.", "** into i, calculated in f"],
      ["lv_r = lv_n20. WRITE / lv_r.", "n into i"],
      ["lv_f = '1E19'. lv_r8 = lv_f. WRITE / lv_r8.", "f into int8"],
      ["lv_r8 = '99999999999999999999'. WRITE / lv_r8.", "text into int8"],
      ["lv_r8 = lv_n20. WRITE / lv_r8.", "n into int8"],
    ];
    for (const [statement, name] of cases) {
      it(name, async () => {
        const out = await output(tries(declarations, [statement], "cx_sy_conversion_overflow"));
        expect(out).to.equal("CX_SY_CONVERSION_OVERFLOW");
      });
    }

    it("rounding comes first", async () => {
      const out = await output(tries(`
DATA lv_r TYPE i.
DATA lv_f TYPE f.
DATA lv_p TYPE p LENGTH 8 DECIMALS 1.`, [
        "lv_f = '2147483647.4'. lv_r = lv_f. WRITE / lv_r.",
        "lv_f = '2147483647.5'. lv_r = lv_f. WRITE / lv_r.",
        "lv_p = '-2147483648.4'. lv_r = lv_p. WRITE / lv_r.",
        "lv_p = '-2147483648.5'. lv_r = lv_p. WRITE / lv_r.",
      ], "cx_sy_conversion_overflow"));
      expect(out).to.equal("2147483647\nCX_SY_CONVERSION_OVERFLOW\n-2147483648\nCX_SY_CONVERSION_OVERFLOW");
    });
  });

});
