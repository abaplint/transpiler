// import {expect} from "chai";
import {ABAP} from "../../packages/runtime/src";
import {AsyncFunction, runFiles} from "../_utils";

let abap: ABAP;

async function run(contents: string) {
  return runFiles(abap, [{filename: "zfoobar.prog.abap", contents}]);
}

describe("Builtin functions - to_mixed", () => {

  beforeEach(async () => {
    abap = new ABAP();
  });

  it("test1", async () => {
    const code = `
    DATA m TYPE string.
    m = to_mixed( val = |Hello World| ).
    ASSERT m = 'Hello world'.
    `;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
  });

  it("test2", async () => {
    const code = `
  DATA m TYPE string.
  m = to_mixed( val = |Hello_World| ).
  ASSERT m = 'HelloWorld'.
  `;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
  });

  it("test3", async () => {
    const code = `
  DATA m TYPE string.
  m = to_mixed( val = |Hello World| sep  = | | ).
  ASSERT m = 'HelloWorld'.
  `;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
  });

  it("test4", async () => {
    const code = `
  DATA m TYPE string.
  m = to_mixed( val = |Hello World| sep  = | | case = 'a' ).
  ASSERT m = 'helloWorld'.
  `;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
  });

  it("test5", async () => {
    const code = `
  DATA m TYPE string.
  m = to_mixed( val = |Hello World| sep  = | | case = 'A' ).
  ASSERT m = 'HelloWorld'.
  `;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
  });

  it.skip("test6", async () => {
    const code = `
  ASSERT |HelloTo world| = to_mixed( val = |Hello to world| sep  = | | case = 'A' min = 3 ).
  `;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
  });

  it("test7", async () => {
    const code = `
  ASSERT |HelloToWorld| = to_mixed( val = |Hello to world| sep  = | | case = 'A' ).
  `;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
  });

});
