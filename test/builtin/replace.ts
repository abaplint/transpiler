import {expect} from "chai";
import {ABAP} from "../../packages/runtime/src";
import {AsyncFunction, runFiles} from "../_utils";

let abap: ABAP;

async function run(contents: string) {
  return runFiles(abap, [{filename: "zfoobar.prog.abap", contents}]);
}

describe("Builtin functions - replace", () => {

  beforeEach(async () => {
    abap = new ABAP();
  });

  it("replace 01", async () => {
    const code = `
    DATA result TYPE string.
    result = replace( val = 'hello' sub = 'l' with = 'o' ).
    WRITE result.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("heolo");
  });

  it("replace 02", async () => {
    const code = `
    DATA result TYPE string.
    result = replace( val = 'hello' sub = 'l' with = 'o' occ = 0).
    WRITE result.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("heooo");
  });

  it("replace 03", async () => {
    const code = "DATA lv_exp TYPE string VALUE 'bar [ [ foo'.\n" +
      "lv_exp = replace( val = lv_exp sub = `[ ` with = '[' occ = 0 ).\n" +
      "WRITE lv_exp.";
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("bar [[foo");
  });

  it("replace 04", async () => {
    const code = `DATA rv_escaped TYPE string VALUE 'foo\\bar'.
    rv_escaped = replace( val = rv_escaped sub = '\\' with = '\\\\' occ = 0 ).
    WRITE rv_escaped.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("foo\\\\bar");
  });

  it("replace with regex", async () => {
    const code = `data text type string.
text = replace( val   = to_lower( 'O M G' )
                regex = \`[ .,]\`
                with  = \`\`
                occ   = 0 ).
write text.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("omg");
  });

  it.only("replace with offset", async () => {
    const code = `DATA str TYPE string.
    str = 'sdfsdfsd'.
    str = replace(
      val = str
      off = 2
      len = 1
      with = \`\` ).
    ASSERT str = 'sdsdfsd'.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
  });

});
