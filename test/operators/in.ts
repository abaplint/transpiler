// import {expect} from "chai";
import {ABAP} from "../../packages/runtime/src";
import {AsyncFunction, runFiles} from "../_utils";

let abap: ABAP;

async function run(contents: string) {
  return runFiles(abap, [{filename: "zfoobar.prog.abap", contents}]);
}

describe("Running operators - IN", () => {

  beforeEach(async () => {
    abap = new ABAP();
  });

  it("IN empty", async () => {
    const code = `
  DATA bar TYPE RANGE OF i.
  ASSERT 5 IN bar.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
  });

  it("NOT IN", async () => {
    const code = `
  DATA bar TYPE RANGE OF i.
  FIELD-SYMBOLS <moo> LIKE LINE OF bar.
  APPEND INITIAL LINE TO bar ASSIGNING <moo>.
  <moo>-sign = 'I'.
  <moo>-option = 'EQ'.
  <moo>-low = 2.
  ASSERT 5 NOT IN bar.
  ASSERT NOT 5 IN bar.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
  });

  it("2 IN 2", async () => {
    const code = `
  DATA bar TYPE RANGE OF i.
  FIELD-SYMBOLS <moo> LIKE LINE OF bar.
  APPEND INITIAL LINE TO bar ASSIGNING <moo>.
  <moo>-sign = 'I'.
  <moo>-option = 'EQ'.
  <moo>-low = 2.
  ASSERT 2 IN bar.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
  });

  it("IN with I CP", async () => {
    const code = `
DATA bar TYPE RANGE OF string.
FIELD-SYMBOLS <moo> LIKE LINE OF bar.
APPEND INITIAL LINE TO bar ASSIGNING <moo>.
<moo>-sign = 'I'.
<moo>-option = 'CP'.
<moo>-low = '*hello*'.
ASSERT 'hello world' IN bar.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
  });

});