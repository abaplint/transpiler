import {expect} from "chai";
import {ABAP, MemoryConsole} from "../../packages/runtime/src";
import {AsyncFunction, runFiles} from "../_utils";

let abap: ABAP;

async function run(contents: string) {
  return runFiles(abap, [{filename: "zfoobar.prog.abap", contents}]);
}

describe("Builtin functions - shift_left", () => {

  beforeEach(async () => {
    abap = new ABAP({console: new MemoryConsole()});
  });

  it("basic shift_left()", async () => {
    const code = "ASSERT shift_left( val = 'aabbcc' sub = `a` ) = 'bbcc'.";
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
  });

  it("places", async () => {
    const code = `
    DATA phrase TYPE string.
    phrase = |abc|.
    phrase = shift_left( val = phrase places = 1 ).
    WRITE phrase.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("bc");
  });

  it("circular", async () => {
    const code = `
    DATA phrase TYPE string.
    phrase = |abc|.
    phrase = shift_left( val = phrase circular = 1 ).
    WRITE phrase.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("bca");
  });

  it("circular overflow", async () => {
    const code = `
    DATA phrase TYPE string.
    phrase = |abc|.
    phrase = shift_left( val = phrase circular = 4 ).
    WRITE phrase.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("bca");
  });

  it("remote whitespace", async () => {
    const code = `
    DATA lv_str TYPE string.
    lv_str = | sdf|.
    ASSERT strlen( shift_left( lv_str ) ) = 3.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
  });
});
