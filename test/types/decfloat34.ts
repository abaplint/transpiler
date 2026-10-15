import {expect} from "chai";
import {ABAP, MemoryConsole} from "../../packages/runtime/src/";
import {AsyncFunction, runFiles} from "../_utils";

let abap: ABAP;

async function run(contents: string) {
  return runFiles(abap, [{filename: "zdecfloat34.prog.abap", contents}]);
}

describe("Running Examples - Decfloat34 type", () => {

  beforeEach(async () => {
    abap = new ABAP({console: new MemoryConsole()});
  });

  it("basic", async () => {
    const code = `
DATA foo TYPE decfloat34.
foo = 1.
WRITE / foo.

foo = '1.12345'.
WRITE / foo.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("1\n1,12345");
  });

  it("multiply", async () => {
    const code = `
    DATA dec TYPE decfloat34.
    dec = 1 / 2.
    dec = dec * 10.
    WRITE / dec.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("5");
  });

  it("compare, LT", async () => {
    const code = `
    DATA val1 TYPE decfloat34.
    DATA val2 TYPE decfloat34.
    val1 = 20.
    val2 = 100.
    ASSERT val1 LT val2.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
  });

  it("compare, LT, field symbols", async () => {
    const code = `
    DATA val1 TYPE decfloat34.
    DATA val2 TYPE decfloat34.
    FIELD-SYMBOLS <fs1> TYPE decfloat34.
    FIELD-SYMBOLS <fs2> TYPE decfloat34.
    val1 = 20.
    val2 = 100.
    ASSIGN val1 TO <fs1>.
    ASSIGN val2 TO <fs2>.
    ASSERT <fs1> LT <fs2>.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
  });

  it("compare, EQ", async () => {
    const code = `
    DATA val1 TYPE decfloat34.
    DATA val2 TYPE decfloat34.
    val1 = 100.
    val2 = 100.
    ASSERT val1 EQ val2.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
  });

  it("compare packed and decfloat34, EQ", async () => {
    const code = `
DATA lv_packed TYPE p LENGTH 8 DECIMALS 3.
DATA lv_decfloat TYPE decfloat34.
lv_packed = '11.500'.
lv_decfloat = '11.5'.
ASSERT lv_packed = lv_decfloat.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
  });

  it("thousand", async () => {
    const code = `
DATA foo TYPE decfloat34.
DATA out TYPE string.
foo = 1000.
out = |{ foo }|.
WRITE / out.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("1000");
  });

  it("parsing", async () => {
    const code = `
DATA foo TYPE decfloat34.
DATA out TYPE string.
foo = '1.123'.
out = |{ foo }|.
WRITE / foo.
WRITE / out.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("1,123\n1.123");
  });

  it("decfloat34, a value written out can be read back", async () => {
    // this one was silent rather than loud: parseFloat stops at the comma
    // this type writes, so reading back 9,79440789 gave 9 and no error
    const code = `
DATA value TYPE decfloat34.
DATA ch TYPE c LENGTH 30.
DATA back TYPE decfloat34.
value = '9.79440789'.
ch = value.
back = ch.
ASSERT back = value.
WRITE / ch.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.getTrimmed()).to.equal("9,79440789");
  });

  it("initial", async () => {
    const code = `
DATA value TYPE decfloat34.
ASSERT value IS INITIAL.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
  });

  
  it("decfloat34 from int8", async () => {
    const code = `
    DATA i8 TYPE int8.
    DATA dec TYPE decfloat34.
    DATA int TYPE i.
    i8 = 1730617950.
    dec = i8.
    int = dec - 1730617900.
    WRITE int.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("50");
  });

  it("decfloat34, exact sum", async () => {
    const code = `
DATA a TYPE decfloat34.
DATA b TYPE decfloat34.
DATA sum TYPE decfloat34.
a = '0.1'.
b = '0.2'.
sum = a + b.
ASSERT sum = '0.3'.
WRITE sum.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("0,3");
  });

  it("decfloat34, 34 significant digits", async () => {
    const code = `
DATA dec TYPE decfloat34.
DATA text TYPE string.
dec = '1234567890123456789012345678901234'.
dec = dec + 1.
text = |{ dec }|.
WRITE text.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("1234567890123456789012345678901235");
  });

  it("decfloat34, division rounded to 34 digits", async () => {
    const code = `
DATA dec TYPE decfloat34.
DATA text TYPE string.
dec = 2.
dec = dec / 3.
text = |{ dec }|.
WRITE text.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("0.6666666666666666666666666666666667");
  });

  it("decfloat34, text that is not a number", async () => {
    const code = `
DATA dec TYPE decfloat34.
dec = CONV decfloat34( 'abc' ).`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    try {
      await f(abap);
      expect.fail();
    } catch (e) {
      expect(e.toString()).to.contain("CX_SY_CONVERSION_NO_NUMBER");
    }
  });

  it("decfloat34, to a packed number with seven decimals", async () => {
    const code = `
DATA dec TYPE decfloat34.
DATA stamp TYPE p LENGTH 11 DECIMALS 7.
dec = '20260915100000.1234567'.
stamp = dec.
WRITE stamp.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("20260915100000,1234567");
  });

  it("decfloat34, frac( ) and floor( ) keep all digits", async () => {
    const code = `
DATA stamp TYPE p LENGTH 11 DECIMALS 7.
DATA dec TYPE decfloat34.
DATA text TYPE string.
stamp = '20260915100000.1234567'.
dec = frac( stamp ).
text = |{ dec }|.
WRITE / text.
dec = stamp.
dec = floor( dec ).
text = |{ dec }|.
WRITE / text.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("0.1234567\n20260915100000");
  });

  it("decfloat34, compared exactly", async () => {
    const code = `
DATA big TYPE decfloat34.
DATA next TYPE decfloat34.
big = '10000000000000000'.
next = '10000000000000001'.
IF next > big.
  WRITE / 'greater'.
ENDIF.
IF next <> big.
  WRITE / 'different'.
ENDIF.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("greater\ndifferent");
  });

  it("decfloat34, to int8 exactly", async () => {
    const code = `
DATA dec TYPE decfloat34.
DATA i8 TYPE int8.
dec = '9007199254740993.4'.
i8 = dec.
WRITE i8.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("9007199254740993");
  });

  it("decfloat34, string template without exponent", async () => {
    const code = `
DATA dec TYPE decfloat34.
DATA text TYPE string.
dec = '0.0000001'.
text = |{ dec }|.
WRITE text.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("0.0000001");
  });

});
