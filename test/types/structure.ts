import {expect} from "chai";
import {ABAP} from "../../packages/runtime/src/";
import {AsyncFunction, runFiles} from "../_utils";

let abap: ABAP;

async function run(contents: string) {
  return runFiles(abap, [{filename: "zfoobar.prog.abap", contents}]);
}

describe("Running Examples - Structure type", () => {

  beforeEach(async () => {
    abap = new ABAP();
  });

  it("Move, non matching columns", async () => {
    const code = `
TYPES: BEGIN OF alphatab_type,
         cola TYPE string,
       END OF alphatab_type.
TYPES: BEGIN OF combined_data_type,
         colx TYPE string,
       END OF combined_data_type.
DATA foo1 TYPE alphatab_type.
DATA foo2 TYPE combined_data_type.
foo1 = foo2.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
  });

  it("Move, cstructure into string", async () => {
    const code = `
TYPES: BEGIN OF ty,
         name TYPE c LENGTH 10,
       END OF ty.
DATA row TYPE ty.
DATA key_name TYPE string.
row-name = 'hello'.
key_name = row.
WRITE key_name.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("hello");
  });

});