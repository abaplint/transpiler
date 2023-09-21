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
                  regex = |\\s*</head>|
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

  it("pcre", async () => {
    const code = `
    DATA val TYPE i.
    val = find( val = 'hello' pcre = 'l' ).
    ASSERT val = 2.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
  });

});
