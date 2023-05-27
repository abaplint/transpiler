import {expect} from "chai";
import {ABAP, MemoryConsole} from "../../packages/runtime/src";
import {AsyncFunction, runFiles} from "../_utils";

let abap: ABAP;

async function run(contents: string) {
  return runFiles(abap, [{filename: "zfoobar.prog.abap", contents}]);
}

describe("Builtin functions - segment", () => {

  beforeEach(async () => {
    abap = new ABAP({console: new MemoryConsole()});
  });

  it("segment 1", async () => {
    const code = `
DATA val TYPE string.
val = segment( val = 'hello world' index = 1 sep = | | ).
WRITE val.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("hello");
  });

  it("segment 2", async () => {
    const code = `
DATA val TYPE string.
val = segment( val = 'hello world' index = 2 sep = | | ).
WRITE val.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("world");
  });

  it("segment, negative index", async () => {
    const code = `
DATA val TYPE string.
val = segment( val = 'hello world' index = -1 sep = | | ).
WRITE val.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("world");
  });

});
