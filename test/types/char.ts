import {expect} from "chai";
import {ABAP, MemoryConsole} from "../../packages/runtime/src/";
import {AsyncFunction, runFiles} from "../_utils";

let abap: ABAP;

async function run(contents: string) {
  return runFiles(abap, [{filename: "zfoobar.prog.abap", contents}]);
}

describe("Running Examples - Character type", () => {

  beforeEach(async () => {
    abap = new ABAP({console: new MemoryConsole()});
  });

  it("compare with new line", async () => {
    const code = `
    DATA foo TYPE string.
    DATA char TYPE c LENGTH 1.
    foo = |\\n|.
    char = foo(1).
    ASSERT char = |\\n|.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
  });

  it("compare with new line", async () => {
    const code = `
DATA: BEGIN OF stru,
        field1 TYPE c LENGTH 2,
        field2 TYPE c LENGTH 2,
      END OF stru.
DATA target TYPE c LENGTH 3.
stru-field1 = '12'.
stru-field2 = '34'.
target = stru.
WRITE target.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("123");
  });

  it("value from int", async () => {
    const code = `
DATA char3 TYPE c LENGTH 3.
DATA str TYPE string.
char3 = 1.
ASSERT char3 = ' 1 '.
CONCATENATE 'hello' char3 INTO str.
WRITE str.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("hello 1");
  });

  it("value from int, 10", async () => {
    const code = `
DATA char10 TYPE c LENGTH 10.
char10 = 1.
ASSERT char10 = '        1 '.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
  });

});