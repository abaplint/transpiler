import {expect} from "chai";
import {ABAP, MemoryConsole} from "../../packages/runtime/src/";
import {AsyncFunction, runFiles} from "../_utils";

let abap: ABAP;

async function run(contents: string) {
  return runFiles(abap, [{filename: "zfoobar.prog.abap", contents}]);
}

describe("Running Examples - Integer8 type", () => {

  beforeEach(async () => {
    abap = new ABAP({console: new MemoryConsole()});
  });

  it("int8", async () => {
    const code = `
  DATA val TYPE int8.
  val = 2.
  WRITE val.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("2");
  });

  it("int8 from large constant", async () => {
    const code = `
  DATA val TYPE int8.
  val = 198620152477517.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
  });

  it("int8 from large constant, to string", async () => {
    const code = `
    DATA lv_int TYPE int8.
    DATA lv_str TYPE string.
    lv_int = 9219994337134247936.
    lv_str = lv_int.
    WRITE lv_str.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("9219994337134247936");
  });

  it("int8 from float", async () => {
    const code = `
    DATA float TYPE f.
    DATA int8 TYPE int8.
    float = 1.
    int8 = float.
    WRITE / int8.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("1");
  });

  it("int8 from int", async () => {
    const code = `
    DATA int TYPE i.
    DATA int8 TYPE int8.
    int = 1.
    int8 = int.
    WRITE / int8.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("1");
  });

  it("int8 from int, plus", async () => {
    const code = `
    DATA int TYPE i.
    DATA int8 TYPE int8.
    int = 1.
    int8 = int8 + int.
    WRITE / int8.
    int8 = int + int8.
    WRITE / int8.
    int8 = int8 + int8.
    WRITE / int8.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("1\n2\n4");
  });

  it("int8 from int, minus", async () => {
    const code = `
    DATA int TYPE i.
    DATA int8 TYPE int8.
    int8 = 2.
    int = 1.
    int8 = int8 - int.
    WRITE / int8.
    int8 = int - int8.
    WRITE / int8.
    int8 = int8 - int8.
    WRITE / int8.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("1\n0\n0");
  });

// minus + mod + multiply + div + power

});