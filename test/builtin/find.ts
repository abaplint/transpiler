import {expect} from "chai";
import {ABAP, MemoryConsole} from "../../packages/runtime/src";
import {AsyncFunction, runFiles} from "../_utils";

let abap: ABAP;

async function run(contents: string) {
  return runFiles(abap, [{filename: "zfoobar.prog.abap", contents}]);
}

describe("Builtin functions - find", () => {

  beforeEach(async () => {
    abap = new ABAP({console: new MemoryConsole()});
  });

  it("find 01", async () => {
    const code = `
    DATA str TYPE string.
    DATA off TYPE i.
    str = 'foobar'.
    off = find( val = str sub = 'oo' off = 0 ).
    WRITE off.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("1");
  });

  it("find 02", async () => {
    const code = `
    DATA str TYPE string.
    DATA off TYPE i.
    str = 'foobar'.
    off = find( val = str sub = 'oo' off = 3 ).
    WRITE off.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("-1");
  });

  it("find 03", async () => {
    const code = `
DATA lv_end TYPE i.
lv_end = find( val = 'aa' regex = |aa| case = abap_false ).
WRITE lv_end.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("0");
  });

  it("find 04, case", async () => {
    const code = `
DATA lv_end TYPE i.
lv_end = find( val = 'aa' regex = |AA| case = abap_false ).
WRITE lv_end.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("0");
  });

  it("find 05, via regex, not found", async () => {
    const code = `
DATA lv_end TYPE i.
lv_end = find( val = 'aa' regex = |bb| case = abap_false ).
WRITE lv_end.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("-1");
  });

  it("find 06", async () => {
    const code = `
DATA lv_offset TYPE i.
DATA lv_html TYPE string.

lv_html = '<!DOCTYPE html><html><head><title>abapGit</title><link rel="stylesheet" type="text/css"' &&
          'href="css/common.css"><link rel="stylesheet" type="text/css" href="css/ag-icons.css">' &&
          '<link rel="stylesheet" type="text/css" href="css/theme-default.css"><script type="text/javascript"' &&
          ' src="js/common.js"></script></head>'.

lv_offset = find( val = lv_html
                  regex = |\\\\s*</head>|
                  case = abap_false ).

WRITE lv_offset.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("299");
  });

  it("not found", async () => {
    const code = `
    DATA path TYPE string.
    DATA res TYPE i.
    path = 'foobarmoo'.
    res = find( val = path sub = '/' ).
    WRITE res.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("-1");
  });

  it("find wich occ positive", async () => {
    const code = `
    DATA path TYPE string.
    DATA res TYPE i.
    path = 'foo/barr/moo'.
    res = find( val = path sub = '/' occ = 2 ).
    WRITE res.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("8");
  });

  it("find wich occ positive, 3", async () => {
    const code = `
    DATA path TYPE string.
    DATA res TYPE i.
    path = 'foo/barr/moosdfsdf/'.
    res = find( val = path sub = '/' occ = 3 ).
    WRITE res.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("18");
  });

  it("find wich occ negative, two", async () => {
    const code = `
    DATA path TYPE string.
    DATA res TYPE i.
    path = 'foo/barr/sdfddmoo'.
    res = find( val = path sub = '/' occ = -2 ).
    WRITE res.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("3");
  });

  it("find wich occ negative, one", async () => {
    const code = `
    DATA path TYPE string.
    DATA res TYPE i.
    path = 'foo/barr/moo'.
    res = find( val = path sub = '/' occ = -1 ).
    WRITE res.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("8");
  });

  it("find wich occ negative, many", async () => {
    const code = `
    DATA path TYPE string.
    DATA res TYPE i.
    path = 'foo/barr/moo'.
    res = find( val = path sub = '/' occ = -10 ).
    WRITE res.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("-1");
  });

  it("find wich occ negative, first", async () => {
    const code = `
    DATA res TYPE i.
    res = find(
      val = '/test/path/file.xml'
      sub = '/'
      occ = -3 ).
    WRITE res.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("0");
  });

  it("find wich occ negative, first", async () => {
    const code = `
    DATA res TYPE i.
    res = find(
      val = '/test/path/file.xml'
      sub = '/'
      occ = 2 ).
    WRITE res.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("5");
  });

  it("find, case = abap_false with sub", async () => {
    const code = `
    DATA lv TYPE i.
    lv = find( val = 'Hello World' sub = 'WORLD' case = abap_false ).
    ASSERT lv = 6.
    lv = find( val = '{"startDate":"2018"}' sub = '"startdate":"' case = abap_false ).
    ASSERT lv = 1.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
  });

  it("find, case = abap_true with sub stays case sensitive", async () => {
    const code = `
    DATA lv TYPE i.
    lv = find( val = 'Hello World' sub = 'WORLD' case = abap_true ).
    ASSERT lv = -1.
    lv = find( val = 'Hello World' sub = 'WORLD' ).
    ASSERT lv = -1.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
  });

  it("find, case = abap_false with sub and occ", async () => {
    const code = `
    DATA lv TYPE i.
    lv = find( val = 'aXbxcX' sub = 'x' occ = 2 case = abap_false ).
    ASSERT lv = 3.
    lv = find( val = 'aXbxcX' sub = 'x' occ = -1 case = abap_false ).
    ASSERT lv = 5.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
  });

  it("find with occ negative, sub longer than one character", async () => {
    const code = `
    DATA lv TYPE i.
    lv = find( val = 'abcab' sub = 'ab' occ = -1 ).
    ASSERT lv = 3.
    lv = find( val = 'abcab' sub = 'ab' occ = -2 ).
    ASSERT lv = 0.
    lv = find( val = 'abcab' sub = 'ab' occ = -3 ).
    ASSERT lv = -1.
    lv = find( val = 'foo/barr/moo' sub = 'rr/' occ = -1 ).
    ASSERT lv = 6.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
  });

  it("find with occ negative and off", async () => {
    const code = `
    DATA lv TYPE i.
    lv = find( val = 'abcabcab' sub = 'ab' off = 1 occ = -1 ).
    ASSERT lv = 6.
    lv = find( val = 'abcabcab' sub = 'ab' off = 1 occ = -2 ).
    ASSERT lv = 3.
    lv = find( val = 'abcabcab' sub = 'ab' off = 1 occ = -3 ).
    ASSERT lv = -1.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
  });

  it("pcre", async () => {
    const code = `
    DATA val TYPE i.
    val = find( val = 'hello' pcre = 'l' ).
    ASSERT val = 2.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
  });

  it("FIND FIRST OCCURRENCE treats anchors as literals", async () => {
    const code = `
    DATA offset TYPE i.
    FIND FIRST OCCURRENCE OF '$' IN 'abc' MATCH OFFSET offset.
    ASSERT sy-subrc = 4.
    FIND FIRST OCCURRENCE OF '^' IN 'a^b' MATCH OFFSET offset.
    ASSERT sy-subrc = 0.
    ASSERT offset = 1.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
  });

  it("off from nmin, two digits", async () => {
    const code = `
    DATA lv TYPE i.
    lv = find( val = 'zaaaaaaaaaaaaaaz' sub = 'z' off = nmin( val1 = 12 val2 = 30 ) ).
    ASSERT lv = 15.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
  });

});
