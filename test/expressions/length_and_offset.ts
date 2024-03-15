import {expect} from "chai";
import {ABAP, MemoryConsole} from "../../packages/runtime/src";
import {AsyncFunction, runFiles} from "../_utils";

let abap: ABAP;

async function run(contents: string) {
  return runFiles(abap, [{filename: "zfoobar.prog.abap", contents}]);
}

describe("Running expressions - Length and offset", () => {

  beforeEach(async () => {
    abap = new ABAP({console: new MemoryConsole()});
  });

  it("Offset +1", async () => {
    const code = `
      DATA: bar TYPE string.
      bar = 'abc'.
      WRITE bar+1.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("bc");
  });

  it("Length (1)", async () => {
    const code = `
      DATA: bar TYPE string.
      bar = 'abc'.
      WRITE bar(1).`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("a");
  });

  it("hex offset and length, reading", async () => {
    const code = `
      DATA x TYPE xstring.
      x = '123456'.
      WRITE / x+1.
      WRITE / x(1).`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("3456\n12");
  });

  it("source field lengths and offsets", async () => {
    const code = `
      DATA bar TYPE string VALUE '12345'.
      DATA len TYPE i.
      len = 2.
      WRITE / bar+len.
      WRITE / bar(len).
      WRITE / bar+2.
      WRITE / bar(2).`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("345\n12\n345\n12");
  });

  it("target length and offsets", async () => {
    const code = `
      DATA foo TYPE c LENGTH 10.
      foo = '11223355'.
      foo+5(1) = 'A'.
      WRITE / foo.
      foo(1) = 'B'.
      WRITE / foo.
      foo+3 = 'C'.
      WRITE / foo.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.getTrimmed()).to.equal("11223A55\nB1223A55\nB12C");
  });

  it("xstring offset and length", async () => {
    const code = `
      DATA lv_a TYPE i VALUE 2.
      DATA lv_x TYPE xstring VALUE '0F0F0F'.
      lv_a = lv_a + lv_x+1(1).
      WRITE lv_a.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("17");
  });

  it("Field offsets and lengths with structures, source", async () => {
    const code = `
      TYPES:
        BEGIN OF ty_struct,
          num TYPE i,
        END OF ty_struct.

      DATA number TYPE i.
      DATA struct TYPE ty_struct.
      DATA test_string TYPE string.

      test_string = '0123456789'.
      number = 3.
      struct-num = 4.

      WRITE / test_string+1(2).
      WRITE / test_string+2(number).
      WRITE / test_string+3(struct-num).

      WRITE / test_string+number(2).
      WRITE / test_string+number(number).
      WRITE / test_string+number(struct-num).

      WRITE / test_string+struct-num(2).
      WRITE / test_string+struct-num(number).
      WRITE / test_string+struct-num(struct-num).

      sy-index = 8. " This should probably not be allowed... :)
      WRITE / test_string+sy-index(1).`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("12\n234\n3456\n34\n345\n3456\n45\n456\n4567\n8");
  });

  it("Field offsets and lengths with structures, target", async () => {
    const code = `
      TYPES:
        BEGIN OF ty_struct,
          num TYPE i,
        END OF ty_struct.

      DATA number TYPE i.
      DATA struct TYPE ty_struct.
      DATA test_string TYPE c LENGTH 100.

      test_string = '0123456789012'.
      number = 3.
      struct-num = 4.

      test_string+1(2) = '##########'.
      test_string+4(number) = '##########'.
      test_string+8(struct-num) = '##########'.
      WRITE / test_string.

      test_string = '0123456789012'.
      test_string+number(struct-num) = '!!!!!!!!!!'.
      WRITE / test_string.
      test_string+number(number) = '$$$$$$$$$$'.
      WRITE / test_string.
      test_string+number(2) = '££££££££££'.
      WRITE / test_string.

      test_string = '0123456789012'.
      test_string+struct-num(struct-num) = 'PPPPPPPPPP'.
      WRITE / test_string.
      test_string+struct-num(number) = 'AAAAAAAAAA'.
      WRITE / test_string.
      test_string+struct-num(2) = 'ABABABABAB'.
      WRITE / test_string.

      sy-index = 4. " This should probably not be allowed... :)
      WRITE / test_string+sy-index(sy-index).`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.getTrimmed()).to.equal("0##3###7####2\n012!!!!789012\n012$$$!789012\n012££$!789012\n0123PPPP89012\n0123AAAP89012\n0123ABAP89012\nABAP");
  });

  it("Field offsets and lengths with field-symbols, source", async () => {
    const code = `
      TYPES:
        BEGIN OF ty_struct,
          num TYPE i,
        END OF ty_struct.

      DATA number TYPE i.
      DATA struct TYPE ty_struct.
      DATA test_string TYPE string.
      FIELD-SYMBOLS <number> TYPE i.
      FIELD-SYMBOLS <struct> TYPE ty_struct.

      test_string = '0123456789'.
      number = 3.
      struct-num = 4.
      ASSIGN number TO <number>.
      ASSIGN struct TO <struct>.

      WRITE / test_string+2(<number>).
      WRITE / test_string+3(<struct>-num).

      WRITE / test_string+<number>(2).
      WRITE / test_string+<number>(<number>).
      WRITE / test_string+<number>(<struct>-num).

      WRITE / test_string+<struct>-num(2).
      WRITE / test_string+<struct>-num(<number>).
      WRITE / test_string+<struct>-num(<struct>-num).`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("234\n3456\n34\n345\n3456\n45\n456\n4567");
  });

  it("Field offsets and lengths with field-symbols, target", async () => {
    const code = `
      TYPES:
        BEGIN OF ty_struct,
          num TYPE i,
        END OF ty_struct.

      DATA number TYPE i.
      DATA struct TYPE ty_struct.
      DATA test_string TYPE c LENGTH 100.
      FIELD-SYMBOLS <number> TYPE i.
      FIELD-SYMBOLS <struct> TYPE ty_struct.

      test_string = '0123456789012'.
      number = 3.
      struct-num = 4.
      ASSIGN number TO <number>.
      ASSIGN struct TO <struct>.

      test_string+1(2) = '##########'.
      test_string+4(<number>) = '##########'.
      test_string+8(<struct>-num) = '##########'.
      WRITE / test_string.

      test_string = '0123456789012'.
      test_string+<number>(<struct>-num) = '!!!!!!!!!!'.
      WRITE / test_string.
      test_string+<number>(<number>) = '$$$$$$$$$$'.
      WRITE / test_string.
      test_string+<number>(2) = '££££££££££'.
      WRITE / test_string.

      test_string = '0123456789012'.
      test_string+<struct>-num(<struct>-num) = 'PPPPPPPPPP'.
      WRITE / test_string.
      test_string+<struct>-num(<number>) = 'AAAAAAAAAA'.
      WRITE / test_string.
      test_string+<struct>-num(2) = 'ABABABABAB'.
      WRITE / test_string.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.getTrimmed()).to.equal("0##3###7####2\n012!!!!789012\n012$$$!789012\n012££$!789012\n0123PPPP89012\n0123AAAP89012\n0123ABAP89012");
  });

  it("getOffset for field-symbols", async () => {
    const code = `
    DATA lv_row TYPE string.
    FIELD-SYMBOLS <lv_row> TYPE string.
    lv_row = |foobar|.
    ASSIGN lv_row TO <lv_row>.
    <lv_row> = <lv_row>(3).
    WRITE <lv_row>.
    <lv_row> = |foobar|.
    <lv_row> = <lv_row>+3(3).
    WRITE / <lv_row>.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("foo\nbar");
  });

  it("field offset and length inside string template", async () => {
    const code = `
    DATA text TYPE string VALUE 'HEYABAPPALOBA'.
    WRITE |{ text+3(4) }|.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("ABAP");
  });

  it("field offset and length for date", async () => {
    const code = `
    DATA date type d.
    date = '19991231'.
    WRITE |{ date+0(4) }-{ date+4(2) }-{ date+6(2) }|.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("1999-12-31");
  });

  it("field offset and length for time", async () => {
    const code = `
    DATA time type t.
    time = '123456'.
    WRITE |{ time+0(2) }:{ time+2(2) }:{ time+4(2) }|.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("12:34:56");
  });

  it("length zero", async () => {
    const code = `
  DATA foo TYPE string.
  foo = 'abc'.
  foo = foo(0).
  ASSERT foo = ''.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
  });

  it("ofset zero", async () => {
    const code = `
  DATA foo TYPE string.
  foo = 'abc'.
  foo = foo+0.
  ASSERT foo = 'abc'.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
  });

  it("offset int set into hex", async () => {
    const code = `
  DATA rv_s TYPE x LENGTH 10.
  DATA lv_int TYPE i.
  lv_int = 8.
  rv_s+2(1) = lv_int.
  WRITE / rv_s.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("00000800000000000000");
  });

  it("offset int set into hex, 16, 1", async () => {
    const code = `
  DATA rv_s TYPE x LENGTH 10.
  DATA lv_int TYPE i.
  lv_int = 16.
  rv_s+2(1) = lv_int.
  WRITE / rv_s.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("00001000000000000000");
  });

  it("offset int set into hex, 16, 2", async () => {
    const code = `
  DATA rv_s TYPE x LENGTH 10.
  rv_s+2(1) = 16.
  WRITE / rv_s.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("00001000000000000000");
  });

  it("Set in uninitialized area", async () => {
    const code = `
    DATA res TYPE c LENGTH 26.
    DATA pos TYPE i.
    res+pos(1) = abap_true.
    pos = 5.
    res+pos(1) = abap_true.
    ASSERT res = 'X    X                    '.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
  });

  it("Dont loose spaces", async () => {
    const code = `
    DATA row TYPE c LENGTH 10.
    row = ' * '.
    row(1) = '1'.
    ASSERT row+2(1) = space.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
  });

  it("upper case", async () => {
    const code = `
    DATA garden_row TYPE c LENGTH 10.
    DATA plant_number TYPE i.
    plant_number = 1.
    garden_row = 'SDFSDFSD'.
    WRITE GARDEN_ROW+PLANT_NUMBER(1).`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
  });

  it("hex based offset", async () => {
    const code = `
    DATA xstr TYPE xstring.
    DATA hex100 TYPE x LENGTH 100.
    DATA hex TYPE x LENGTH 1.
    hex = '11'.
    DO 100 TIMES.
      CONCATENATE xstr hex INTO xstr IN BYTE MODE.
    ENDDO.
    hex100 = xstr.
    hex100+10(1) = '22'.
    hex = '0A'.
    WRITE / hex100+hex(1).`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("22");
  });

  it("structure based offset and length", async () => {
    const code = `
  TYPES: BEGIN OF ty,
         field1 TYPE c LENGTH 2,
         field2 TYPE c LENGTH 2,
       END OF ty.
  DATA dat TYPE ty.
  dat = '1111'.
  dat+1(2) = 'AB'.
  WRITE / dat.
  WRITE / dat+1(2).`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("1AB1\nAB");
  });

  it("more numeric field names", async () => {
    const code = `
DATA: BEGIN OF hex,
        01 TYPE x LENGTH 1,
        11 TYPE x LENGTH 1,
      END   OF hex.
DATA val TYPE c LENGTH 10.
WRITE val+hex-01(1).`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
  });

  it("more numeric field names", async () => {
    const code = `
DATA char TYPE c LENGTH 10.
char+2(*) = 'sdf'.
WRITE char+2(*).`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get().trim()).to.equal("sdf");
  });

  it("short", async () => {
    const code = `
    DATA foo TYPE c LENGTH 4.
    DATA bar TYPE c LENGTH 1.
    bar = 'A'.
    foo = '1234'.
    foo+1(2) = bar.
    WRITE foo.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get().trim()).to.equal("1A 4");
  });

  it("long", async () => {
    const code = `
    DATA foo TYPE c LENGTH 4.
    DATA bar TYPE c LENGTH 5.
    bar = 'A'.
    foo = '1234'.
    foo+1(2) = bar.
    WRITE foo.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get().trim()).to.equal("1A 4");
  });

});