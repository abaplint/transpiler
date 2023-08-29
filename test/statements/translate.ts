import {expect} from "chai";
import {ABAP, MemoryConsole} from "../../packages/runtime/src";
import {AsyncFunction, runFiles} from "../_utils";

let abap: ABAP;

async function run(contents: string) {
  return runFiles(abap, [{filename: "zfoobar.prog.abap", contents}]);
}

describe("Running statements - TRANSLATE", () => {

  beforeEach(async () => {
    abap = new ABAP({console: new MemoryConsole()});
  });

  it("TRANSLATE to upper case", async () => {
    const code = `
  DATA foo TYPE string.
  foo = 'abc'.
  TRANSLATE foo TO UPPER CASE.
  WRITE foo.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("ABC");
  });

  it("TRANSLATE to lower case", async () => {
    const code = `
    DATA foo TYPE string.
    foo = 'ABC'.
    TRANSLATE foo TO LOWER CASE.
    WRITE foo.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("abc");
  });

  it("TRANSLATE USING", async () => {
    const code = `
DATA str TYPE string.
str = 'apps/create-from-manifest'.
TRANSLATE str USING '/_-_'.
WRITE str.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("apps_create_from_manifest");
  });

  it("TRANSLATE USING, str", async () => {
    const code = `
    DATA str TYPE string.
    DATA using TYPE string.
    using = '/_-_'.
    str = 'apps/create-from-manifest'.
    TRANSLATE str USING using.
    WRITE str.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("apps_create_from_manifest");
  });

  it("TRANSLATE first character", async () => {
    const code = `
  TYPES ty_token TYPE c LENGTH 255.
  DATA lt_tokens TYPE STANDARD TABLE OF ty_token.
  DATA rv_result TYPE string.
  FIELD-SYMBOLS <token> LIKE LINE OF lt_tokens.
  SPLIT 'hello_world' AT '_' INTO TABLE lt_tokens.
  LOOP AT lt_tokens ASSIGNING <token>.
    TRANSLATE <token>(1) TO UPPER CASE.
  ENDLOOP.
  CONCATENATE LINES OF lt_tokens INTO rv_result.
  WRITE rv_result.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("HelloWorld");
  });

  it("TRANSLATE USING special characters", async () => {
    const code = `
    DATA lv_special_chars(50) TYPE c VALUE '. _ ! " & / = + : , - ( ) # @ % ^ $ | ~ '.
    DATA plain_text_lc TYPE string.
    plain_text_lc = 'foo'.
    TRANSLATE plain_text_lc USING lv_special_chars.
    ASSERT plain_text_lc = 'foo'.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
  });

  it("more special characters", async () => {
    const code = `
CONSTANTS c_symbols TYPE string VALUE \` _/_\\_:_;_~_._,_-_+_=_>_<_|_(_)_[_]_{_}_@_+_*_?_!_&_$_#_%_^_'_§_\`.
DATA name TYPE string.
name = 's d~f'.
TRANSLATE name USING c_symbols.
WRITE name.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("s_d_f");
  });

  it("empty character", async () => {
    const code = `
DATA str TYPE string.
str = translate(
  val  = '13:30:00'
  from = ':'
  to   = ' ' ).
WRITE str.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("133000");
  });

  it("only replace once", async () => {
    const code = `
DATA s TYPE string.
s = 'A'.
TRANSLATE s USING 'ABBC'.
WRITE s.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("B");
  });

});