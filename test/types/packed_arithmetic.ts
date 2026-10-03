import {expect} from "chai";
import {ABAP, MemoryConsole} from "../../packages/runtime/src/";
import {AsyncFunction, runFiles} from "../_utils";
import {Float, Integer, Packed} from "../../packages/runtime/src/types";
import {multiply} from "../../packages/runtime/src/operators/multiply";
import {condense} from "../../packages/runtime/src/builtin/condense";
import {round} from "../../packages/runtime/src/builtin/round";
import {alphaOut} from "../../packages/runtime/src/alpha";
import {templateFormatting} from "../../packages/runtime/src/template_formatting";

async function output(contents: string): Promise<string> {
  const abap = new ABAP({console: new MemoryConsole()});
  const js = await runFiles(abap, [{filename: "zpacked.prog.abap", contents}]);
  await new AsyncFunction("abap", js)(abap);
  return abap.console.get();
}

describe("Stored packed arithmetic", () => {
  const cases: [string, string, string][] = [
    ["#500 product", "p = 94906267. p = p * 94906267.", "9007199515875289"],
    ["#500 MOD", "p = 94906267. p = p * 94906267. p = p MOD 4294967296.", "261134297"],
    ["negative product", "p = -94906267. p = p * 94906267.", "-9007199515875289"],
    ["negative MOD", "p = '-9007199515875289'. p = p MOD 4294967296.", "4033832999"],
    ["negative divisor MOD", "p = '9007199515875289'. p = p MOD -4294967296.", "261134297"],
    ["DIV", "p = '9007199515875289'. p = p DIV 94906267.", "94906267"],
    ["negative DIV", "p = '-9007199515875289'. p = p DIV 94906266.", "-94906269"],
    ["addition", "p = '9007199254740992'. p = p + 1.", "9007199254740993"],
    ["negative addition", "p = '-9007199254740992'. p = p - 1.", "-9007199254740993"],
    ["chain", "p = 1000000. p = p * 94906267 * 94906267 MOD 4294967296.", "285403200"],
  ];
  for (const [name, body, expected] of cases) {
    it(name, async () => {
      expect(await output(`DATA p TYPE p LENGTH 16 DECIMALS 0. ${body} WRITE p.`)).to.equal(expected);
    });
  }
  for (const [value, expected] of [["1.25", "1,3"], ["-1.25", "-1,3"]]) {
    it(`smaller p scale rounds half away from zero for ${value}`, async () => {
      expect(await output(`DATA p TYPE p LENGTH 16 DECIMALS 2. DATA q TYPE p DECIMALS 1.
p = '${value}'. q = p * 1. WRITE q.`)).to.equal(expected);
    });
  }
  it("decimal MOD keeps fractional remainder", async () => {
    expect(await output(`DATA p TYPE p DECIMALS 2. p = '7.75'. p = p MOD 2. WRITE p.`)).to.equal("1,75");
  });
  it("mixed p and int8", async () => {
    expect(await output(`DATA p TYPE p LENGTH 16 DECIMALS 1. DATA x TYPE int8.
p = '2.5'. x = 3. p = p * x. WRITE p.`)).to.equal("7,5");
  });
  it("field symbol operand", async () => {
    expect(await output(`DATA p TYPE p LENGTH 16. FIELD-SYMBOLS <p> TYPE p.
p = 94906267. ASSIGN p TO <p>. p = <p> * 94906267. WRITE p.`)).to.equal("9007199515875289");
  });
});

// These values are pinned from upstream/main, including its binary float artifacts.
describe("Packed expression consumers retain main behavior", () => {
  const declaration = "DATA p TYPE p DECIMALS 2. p = '0.10'.";
  const cases: [string, string, string][] = [
    ["WRITE", "WRITE p * 3.", "3,0000000000000004E-01"],
    ["template", "WRITE |{ p * 3 }|.", "0.3000000000000000"],
    ["string target", "DATA s TYPE string. s = p * 3. WRITE s.", "3.0000000000000004E-01"],
    ["c target", "DATA c TYPE c LENGTH 30. c = p * 3. WRITE |[{ c }]|.", "[3,0000000000000004E-01]"],
    ["WIDTH/ALIGN", "WRITE |[{ p * 3 WIDTH = 20 ALIGN = RIGHT }]|.", "[0.3000000000000000  ]"],
    ["f target", "DATA f TYPE f. f = p * 3. WRITE f.", "3,0000000000000004E-01"],
  ];
  for (const [name, body, expected] of cases) {
    it(name, async () => expect(await output(declaration + body)).to.equal(expected));
  }
  it("division into f", async () => {
    expect(await output(`DATA p TYPE p. DATA f TYPE f. p = 1. f = p / 3. WRITE f.`)).to.equal("3,3333333333333331E-01");
  });
  it("ROUND argument into numeric target", async () => {
    expect(await output(`DATA p TYPE p DECIMALS 3. DATA q TYPE p DECIMALS 2.
p = '1.005'. q = round( val = p * 1 dec = 2 ). WRITE q.`)).to.equal("1,00");
  });
  it("CONDENSE, ALPHA OUT, WIDTH/ALIGN and ROUND runtime probes", () => {
    const p = new Packed({decimals: 2}).set("0.10");
    const result = multiply(p, new Integer().set(3)) as Float;
    expect(result).to.be.instanceof(Float);
    expect(condense({val: result as any}).get()).to.equal("3,0000000000000004E-01");
    expect(alphaOut(result as any)).to.equal("3,0000000000000004E-01");
    expect(templateFormatting(result as any, {width: 20, pad: " ", align: "right"})).to.equal("  0.3000000000000000");
    const q = new Packed({decimals: 3}).set("1.005");
    expect(round({val: multiply(q, new Integer().set(1)) as any, dec: 2}).getRaw()).to.equal(1);
  });
});

describe("Packed store boundaries", () => {
  for (const sign of ["", "-"]) {
    for (const [fraction, rounded] of [["24", "2"], ["25", "3"], ["26", "3"]]) {
      for (const expression of ["p", "p * 1"]) {
        it(`exact rescale ${sign}900719925474099.${fraction} via ${expression}`, async () => {
          expect(await output(`DATA p TYPE p LENGTH 16 DECIMALS 2.
DATA q TYPE p LENGTH 16 DECIMALS 1.
p = '${sign}900719925474099.${fraction}'. q = ${expression}. WRITE q.`))
            .to.equal(`${sign}900719925474099,${rounded}`);
        });
      }
    }
    for (const [expression, expected] of [
      ["p / 1", "9007199254740993"],
      ["( p + 1 ) / 1", sign ? "9007199254740992" : "9007199254740994"],
      ["p / 1 + 1", sign ? "9007199254740992" : "9007199254740994"],
    ]) {
      it(`large division ${sign} ${expression}`, async () => {
        expect(await output(`DATA p TYPE p LENGTH 16. p = '${sign}9007199254740993'.
p = ${expression}. WRITE p.`)).to.equal(sign + expected);
      });
    }
    for (const [numerator, denominator, expected] of [["1", "3", "0,33333333333333"],
      ["10", "7", "1,42857142857143"], ["1", "200000000000000", "0,00000000000001"],
      ["1", "200000000000001", "0,00000000000000"]]) {
      it(`fourteen decimal quotient ${sign}${numerator}/${denominator}`, async () => {
        expect(await output(`DATA p TYPE p LENGTH 16 DECIMALS 14.
p = ${sign}${numerator} / ${denominator}. WRITE p.`)).to.equal((expected === "0,00000000000000" ? "" : sign) + expected);
      });
    }
    for (const type of ["i", "int8", "n LENGTH 8"]) {
      it(`${type} operands select packed calculation ${sign}`, async () => {
        const negative = type.startsWith("n") ? "" : sign;
        expect(await output(`DATA a TYPE ${type}. DATA b TYPE ${type}. DATA p TYPE p LENGTH 16.
a = '${negative}94906267'. b = 94906267. p = a * b. WRITE p.`))
          .to.equal(negative + "9007199515875289");
      });
    }
  }
  for (const [a, b, q, r] of [[7, 2, 3, 1], [7, -2, -3, 1], [-7, 2, -4, 1], [-7, -2, 4, 1]]) {
    for (const [op, result] of [["DIV", q], ["MOD", r]]) {
      it(`Euclidean ${a} ${op} ${b}`, async () => {
        expect(await output(`DATA p TYPE p LENGTH 16. p = ${a}. p = p ${op} ${b}. WRITE p.`)).to.equal(String(result));
      });
    }
  }
  for (const a of ["9007199254740993", "-9007199254740993"]) {
    for (const b of [2, -2]) {
      for (const op of ["DIV", "MOD"]) {
        it(`BigInt Euclidean ${a} ${op} ${b}`, async () => {
          const q = a.startsWith("-") ? -4503599627370497n : 4503599627370496n;
          const expected = op === "MOD" ? "1" : (b < 0 ? -q : q).toString();
          expect(await output(`DATA p TYPE p LENGTH 16. p = '${a}'. p = p ${op} ${b}. WRITE p.`)).to.equal(expected);
        });
      }
    }
  }
  for (const sign of ["", "-"]) {
    it(`large integer part with fourteen decimals ${sign}`, async () => {
      expect(await output(`DATA p TYPE p LENGTH 16 DECIMALS 14.
DATA a TYPE p LENGTH 16. a = '${sign}9007199254740993'. p = a / 3. WRITE p.`))
        .to.equal(sign + "3002399751580331,00000000000000");
    });
    it(`ordinary MOVE small negative tie ${sign}`, async () => {
      expect(await output(`DATA p TYPE p DECIMALS 2. DATA q TYPE p DECIMALS 1.
p = '${sign}1.25'. q = p. WRITE q.`)).to.equal(sign + "1,3");
    });
  }
  it("typed packed field symbol target", async () => {
    expect(await output(`DATA p TYPE p LENGTH 16. FIELD-SYMBOLS <p> TYPE p.
ASSIGN p TO <p>. <p> = 94906267 * 94906267. WRITE p.`)).to.equal("9007199515875289");
  });
  it("packed structure component target", async () => {
    expect(await output(`DATA: BEGIN OF s, p TYPE p LENGTH 16, END OF s.
s-p = 94906267 * 94906267. WRITE s-p.`)).to.equal("9007199515875289");
  });
  it("mixed float operand into packed", async () => {
    expect(await output(`DATA p TYPE p DECIMALS 1. DATA f TYPE f.
f = '-1.25'. p = f * 1. WRITE p.`)).to.equal("-1,3");
  });
  it("zero divided by zero", async () => {
    expect(await output(`DATA p TYPE p. p = 0 / 0. WRITE p.`)).to.equal("0");
  });
  for (const sign of ["", "-"]) {
    it(`31 digit boundary ${sign}`, async () => {
      expect(await output(`DATA p TYPE p LENGTH 16. p = '${sign}9999999999999999999999999999999'.
p = p * 1. WRITE p.`)).to.equal(sign + "9999999999999999999999999999999");
    });
  }
});

describe("Packed arithmetic exceptions", () => {
  async function caught(body: string, exception: string): Promise<string> {
    const abap = new ABAP({console: new MemoryConsole()});
    const js = await runFiles(abap, [
      {filename: "zpacked.prog.abap", contents: `TRY. ${body} CATCH ${exception}. WRITE 'caught'. ENDTRY.`},
      {filename: `${exception}.clas.abap`, contents: `CLASS ${exception} DEFINITION PUBLIC.
ENDCLASS. CLASS ${exception} IMPLEMENTATION. ENDCLASS.`},
    ]);
    // The runtime raises the registered global exception constructor.
    abap.Classes[exception.toUpperCase()] = class {} as any;
    await new AsyncFunction("abap", js)(abap);
    return abap.console.get();
  }
  for (const sign of ["", "-"]) {
    for (const [name, body] of [
      ["one digit", `DATA p TYPE p LENGTH 1. p = ${sign}9. p = p ${sign ? "-" : "+"} 1.`],
      ["31 digits", `DATA p TYPE p LENGTH 16. p = '${sign}9999999999999999999999999999999'. p = p ${sign ? "-" : "+"} 1.`],
      ["rounding carries", `DATA p TYPE p LENGTH 1. DATA q TYPE p DECIMALS 1. q = '${sign}9.5'. p = q * 1.`],
      ["MOVE carries", `DATA p TYPE p LENGTH 1. DATA q TYPE p DECIMALS 1. q = '${sign}9.5'. p = q.`],
    ]) {
      it(`catch overflow ${sign} ${name}`, async () => {
        expect(await caught(body, "cx_sy_arithmetic_overflow")).to.equal("caught");
      });
    }
    it(`catch zero divisor ${sign}`, async () => {
      expect(await caught(`DATA p TYPE p. p = ${sign}1 / 0.`, "cx_sy_zerodivide")).to.equal("caught");
    });
    it(`rounded value fits ${sign}`, async () => {
      expect(await output(`DATA p TYPE p LENGTH 1. DATA q TYPE p DECIMALS 2.
q = '${sign}9.49'. p = q * 1. WRITE p.`)).to.equal(sign + "9");
    });
  }
});

describe("Non-packed store dispatch", () => {
  for (const type of ["f", "decfloat34"]) {
    for (const expression of ["a / b * f", "f * a / b", "( a / b ) * ( f * 1 )", "a / b * <f>", "a / b * s-f"]) {
      it(`${type} anywhere selects ordinary dispatch for ${expression}`, async () => {
        const abap = new ABAP({console: new MemoryConsole()});
        const js = await runFiles(abap, [{filename: "zpacked.prog.abap", contents: `
DATA a TYPE i. DATA b TYPE i. DATA f TYPE ${type}.
DATA p TYPE p LENGTH 16 DECIMALS 2.
DATA: BEGIN OF s, f TYPE ${type}, END OF s.
FIELD-SYMBOLS <f> TYPE ${type}.
a = 1. b = 3. f = '100000000000000'. s-f = f. ASSIGN f TO <f>.
p = ${expression}. WRITE p.`}]);
        expect(js).not.to.include("packedOperators");
        await new AsyncFunction("abap", js)(abap);
        expect(abap.console.get()).to.equal("33333333333333,33");
      });
    }
  }
  for (const type of ["i", "int8", "n LENGTH 10", "f", "c LENGTH 30", "string"]) {
    it(`ordinary operators into ${type}`, async () => {
      const abap = new ABAP({console: new MemoryConsole()});
      const js = await runFiles(abap, [{filename: "zpacked.prog.abap", contents:
        `DATA p TYPE p LENGTH 16. DATA q TYPE ${type}. q = p MOD 4294967296.`}]);
      expect(js).to.include("abap.operators.mod(");
      expect(js).not.to.include("packedOperators");
    });
  }
  it("generic field symbol with i target retains ordinary dispatch", async () => {
    const abap = new ABAP({console: new MemoryConsole()});
    const js = await runFiles(abap, [{filename: "zpacked.prog.abap", contents:
      `DATA i TYPE i. FIELD-SYMBOLS <a> TYPE any. ASSIGN i TO <a>. i = <a> + 1. <a> = <a> + 1.`}]);
    expect(js).not.to.include("packedOperators");
    await new AsyncFunction("abap", js)(abap);
  });
});
