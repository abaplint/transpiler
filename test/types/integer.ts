import {expect} from "chai";
import {ABAP} from "../../packages/runtime/src/";
import {AsyncFunction, runFiles} from "../_utils";

let abap: ABAP;

async function run(contents: string) {
  return runFiles(abap, [{filename: "zfoobar.prog.abap", contents}]);
}

describe("Running Examples - Integer type", () => {

  beforeEach(async () => {
    abap = new ABAP();
  });

  it("int", async () => {
    const code = `
  DATA int TYPE i.
  int = 2.
  WRITE int.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("2");
  });

  it("int, negative", async () => {
    const code = `
  DATA int TYPE i.
  int = -2.
  WRITE int.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("-2");
  });

  it("int, positive", async () => {
    const code = `
  DATA int TYPE i.
  int = +4.
  WRITE int.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("4");
  });

  it("no thousand separator, design choice?", async () => {
    const code = `
  DATA int TYPE i.
  int = 2000.
  WRITE int.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("2000");
  });

  it("convert plus 1", async () => {
    const code = `
    DATA int TYPE i.
    int = '+1.0'.
    WRITE int.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("1");
  });

  it("output negative via string", async () => {
    const code = `
    DATA foo TYPE i.
    DATA str TYPE string.
    foo = -68.
    str = foo.
    WRITE str.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("68-");
  });

  it("character spaces", async () => {
    const code = `
    DATA foo TYPE i.
    DATA char TYPE c LENGTH 3.
    char = '3 '.
    foo = char.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
  });
});