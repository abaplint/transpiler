import {ABAP, MemoryConsole} from "../../packages/runtime/src";
import {AsyncFunction, runFiles} from "../_utils";

let abap: ABAP;

async function run(contents: string) {
  return runFiles(abap, [{filename: "zfoobar.prog.abap", contents}]);
}

describe("Builtin math functions", () => {

  beforeEach(async () => {
    abap = new ABAP({console: new MemoryConsole()});
  });

  it("inverse trigonometric functions", async () => {
    const code = `
      ASSERT acos( 1 ) = 0.
      ASSERT acos( 0 ) > 1.
      ASSERT asin( 0 ) = 0.
      ASSERT asin( 1 ) > 1.
      ASSERT atan( 0 ) = 0.
      ASSERT atan( 1 ) > 0.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
  });

  it("hyperbolic functions", async () => {
    const code = `
      ASSERT cosh( 0 ) = 1.
      ASSERT cosh( 1 ) > 1.
      ASSERT sinh( 0 ) = 0.
      ASSERT sinh( 1 ) > 1.
      ASSERT tanh( 0 ) = 0.
      ASSERT tanh( 1 ) > 0.
      ASSERT tanh( 1 ) < 1.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
  });

  it("exponential and logarithmic functions", async () => {
    const code = `
      ASSERT exp( 0 ) = 1.
      ASSERT exp( 1 ) > 2.
      ASSERT log( 1 ) = 0.
      ASSERT log( 3 ) > 1.
      ASSERT log10( 1 ) = 0.
      ASSERT log10( 100 ) = 2.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
  });

});
