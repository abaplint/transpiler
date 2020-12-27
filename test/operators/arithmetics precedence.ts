import {expect} from "chai";
import {ABAP} from "../../packages/runtime/src";
import {runFiles} from "../_utils";

let abap: ABAP;

async function run(contents: string) {
  return runFiles(abap, [{filename: "zfoobar.prog.abap", contents}]);
}

describe("Running operators - arithmetic precedence", () => {

  beforeEach(async () => {
    abap = new ABAP();
  });

  it("Arithmetics, precedence 1", async () => {
    const code = `ASSERT 4 - 0 + 1 = 5.`;
    const js = await run(code);
    const f = new Function("abap", js);
    f(abap);
  });

  it("Arithmetics, precedence 2", async () => {
    const code = `
      DATA int TYPE i.
      int = 2 * 2 + 3 * 3.
      WRITE int.`;
    const js = await run(code);
    const f = new Function("abap", js);
    f(abap);
    expect(abap.console.get()).to.equal("13");
  });

  it("Arithmetics, precedence 3", async () => {
    const code = `ASSERT 100 * 10 + 2 = ( 100 * 10 ) + 2.`;
    const js = await run(code);
    const f = new Function("abap", js);
    f(abap);
  });

  it("Arithmetics, precedence 4", async () => {
    const code = `ASSERT 4 + 1 - 2 = 3.`;
    const js = await run(code);
    const f = new Function("abap", js);
    f(abap);
  });

  it("Arithmetics, precedence 5", async () => {
    const code = `ASSERT 4 + ( 1 - 2 ) = 3.`;
    const js = await run(code);
    const f = new Function("abap", js);
    f(abap);
  });

  it("Arithmetics, precedence 6", async () => {
    const code = `
      DATA int TYPE i.
      int = 4 - 1 - 2.
      WRITE int.`;
    const js = await run(code);
    const f = new Function("abap", js);
    f(abap);
    expect(abap.console.get()).to.equal("1");
  });

  it("Arithmetics, precedence 7", async () => {
    const code = `ASSERT 2 * 2 * 2 = 8.`;
    const js = await run(code);
    const f = new Function("abap", js);
    f(abap);
  });

  it("Arithmetics, precedence 8", async () => {
    const code = `ASSERT 16 / 2 / 2 = 4.`;
    const js = await run(code);
    const f = new Function("abap", js);
    f(abap);
  });

  it("Arithmetics, precedence 9", async () => {
    const code = `
      DATA int TYPE i.
      int = 2 + 2 * 3 + 3.
      WRITE int.`;
    const js = await run(code);
    const f = new Function("abap", js);
    f(abap);
    expect(abap.console.get()).to.equal("11");
  });

});