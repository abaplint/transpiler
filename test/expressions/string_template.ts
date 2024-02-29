import {expect} from "chai";
import {ABAP, MemoryConsole} from "../../packages/runtime/src";
import {AsyncFunction, runFiles} from "../_utils";

let abap: ABAP;

async function run(contents: string) {
  return runFiles(abap, [{filename: "zfoobar.prog.abap", contents}]);
}

describe("Running expressions - String templates", () => {

  beforeEach(async () => {
    abap = new ABAP({console: new MemoryConsole()});
  });

  it("Output TIMESTAMP = ISO", async () => {
    const code = `
    CONSTANTS lc_epoch TYPE p VALUE '19700101000000'.
    WRITE |{ lc_epoch TIMESTAMP = ISO }|.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("1970-01-01T00:00:00");
  });

  it("Output DATE = ISO", async () => {
    const code = `
    CONSTANTS date TYPE d VALUE '20210321'.
    WRITE |{ date DATE = ISO }|.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("2021-03-21");
  });

  it("Output TIME = ISO", async () => {
    const code = `
    CONSTANTS time TYPE t VALUE '112233'.
    WRITE |{ time TIME = ISO }|.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("11:22:33");
  });

  it("Output WIDTH, add spaces", async () => {
    const code = `
    DATA row TYPE c LENGTH 1.
    DATA result TYPE string.
    result = |{ row WIDTH = 10 }|.
    WRITE strlen( result ).`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("10");
  });

  it("Output WIDTH, nothing to add", async () => {
    const code = `
    DATA row TYPE string.
    DATA result TYPE string.
    row = 'abcde'.
    result = |{ row WIDTH = 2 }|.
    WRITE result.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("abcde");
  });

  it("More output WIDTH", async () => {
    const code = `
    DATA row TYPE c LENGTH 255.
    DATA width TYPE i.
    width = 2.
    row(1) = 'a'.
    DATA result TYPE string.
    result = |{ row WIDTH = width }|.
    WRITE strlen( result ).`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("2");
  });

  it("integer DECIMALS", async () => {
    const code = `
    DATA out TYPE string.
    DATA n TYPE i.
    n = 5.
    out = |n = { n DECIMALS = 2 }|.
    WRITE / out.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("n = 5.00");
  });

  it("float DECIMALS", async () => {
    const code = `
    DATA f TYPE f.
    DATA out TYPE string.
    f = '10.239'.
    out = |f = { f DECIMALS = 2 }|.
    WRITE / out.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("f = 10.24");
  });

  it("basic float", async () => {
    const code = `
    DATA f TYPE f.
    DATA out TYPE string.
    f = '10.239'.
    out = |f = { f }|.
    WRITE / out.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
// hmm, something is a bit off?
    expect(abap.console.get()).to.equal("f = 10.2390000000000008");
  });

  it("basic float, integer", async () => {
    const code = `
    DATA f TYPE f.
    DATA out TYPE string.
    f = '10'.
    out = |f = { f }|.
    WRITE / out.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("f = 10");
  });

  it("float DECIMALS, zeros", async () => {
    const code = `
    DATA f TYPE f.
    DATA out TYPE string.
    f = '10.200'.
    out = |f = { f DECIMALS = 2 }|.
    WRITE / out.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("f = 10.20");
  });

  it("WIDTH and PAD", async () => {
    const code = `
  data h type i value 2.
  data m type i value 5.
  write / |{ h WIDTH = 2 }:{ m PAD = '0' }|.
  write / |{ h WIDTH = 2 PAD = '0' }:{ m }|.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("2 :5\n20:5");
  });

  it("WIDTH and PAD and Align", async () => {
    const code = `
    data result type string.
    data hours type i.
    result = |{ hours PAD = '0' WIDTH = 2 ALIGN = RIGHT }|.
    write result.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("00");
  });

  it("more WIDTH and PAD and Align", async () => {
    const code = `
    DATA result TYPE string.
    DATA hours TYPE i.
    DATA minutes TYPE i.
    hours = 2.
    minutes = 2.
    result = |{ hours PAD = '0' WIDTH = 2 ALIGN = RIGHT }:{ minutes PAD = '0' WIDTH = 2 ALIGN = RIGHT }|.
    WRITE result.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("02:02");
  });

  it("packed and DECIMALS", async () => {
    const code = `
TYPES lty_dec2 TYPE p DECIMALS 2.
DATA cost TYPE lty_dec2.
cost = 2.
WRITE |Your sign costs { cost DECIMALS = 2 }|.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("Your sign costs 2.00");
  });

  it("packed and DECIMALS, 2", async () => {
    const code = `
TYPES lty_dec2 TYPE p DECIMALS 2.
DATA cost TYPE lty_dec2.
DATA result TYPE string.
cost = 20.
cost = cost + strlen( \`sdfs\` ) * 2 .
result = |blah { cost DECIMALS = 2 }|.
WRITE result.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("blah 28.00");
  });

  it("return structured field to string template", async () => {
    const code = `
CLASS lcl DEFINITION.
  PUBLIC SECTION.
    TYPES: BEGIN OF ty_structure,
             field TYPE string,
           END OF ty_structure.
    CLASS-METHODS run.
    CLASS-METHODS method RETURNING VALUE(structure) TYPE ty_structure.
ENDCLASS.

CLASS lcl IMPLEMENTATION.
  METHOD run.
    WRITE |{ method( )-field } world|.
  ENDMETHOD.
  METHOD method.
    structure-field = 'hello'.
  ENDMETHOD.
ENDCLASS.

START-OF-SELECTION.
  lcl=>run( ).`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("hello world");
  });

  it("char out", async () => {
    const code = `
DATA lv_str TYPE string.
lv_str = |{ 'sdf   ' }ABC|.
WRITE lv_str.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("sdfABC");
  });

  it("char out, spaces", async () => {
    const code = `
DATA lv_key TYPE c LENGTH 120.
DATA lv_str TYPE string.
lv_key = 'ESHORT               001'.
lv_str = |{ lv_key(10) }ABC|.
WRITE lv_str.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("ESHORTABC");
  });

  it("output field symbol", async () => {
    const code = `
DATA char TYPE c LENGTH 2.
FIELD-SYMBOLS <fs> TYPE any.
char = 'hi'.
ASSIGN char TO <fs>.
WRITE |{ <fs> }|.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("hi");
  });

  it("Output long TIMESTAMP = ISO", async () => {
    const code = `
    DATA ts TYPE p LENGTH 11 DECIMALS 7.
    ts = '19700101000000'.
    WRITE |{ ts TIMESTAMP = ISO }|.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("1970-01-01T00:00:00,0000000");
  });

  it("Output STYLE = SCIENTIFIC", async () => {
    const code = `
DATA lv_value TYPE f.
lv_value = 1.
DO 25 TIMES.
  lv_value = lv_value / 10.
ENDDO.
WRITE |{ lv_value STYLE = SCIENTIFIC }|.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("1.0000000000000002E-25");
  });

});