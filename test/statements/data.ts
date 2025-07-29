import {expect} from "chai";
import {ABAP, MemoryConsole} from "../../packages/runtime/src";
import {AsyncFunction, runFiles} from "../_utils";

let abap: ABAP;

async function run(contents: string) {
  return runFiles(abap, [{filename: "zfoobar.prog.abap", contents}]);
}

describe("Running statements - DATA", () => {

  beforeEach(async () => {
    abap = new ABAP({console: new MemoryConsole()});
  });

  it("DATA, with BEGIN OF", async () => {
    const code = `
DATA: BEGIN OF ls_msg,
        a1 TYPE c LENGTH 1,
        a2 TYPE c LENGTH 1,
        a3 TYPE c LENGTH 1,
        a4 TYPE c LENGTH 1,
      END OF ls_msg.
ls_msg-a3 = 'A'.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
  });

  it("DATA, upper case component name", async () => {
    const code = `
DATA: BEGIN OF ls_msg,
        a3 TYPE c LENGTH 1,
      END OF ls_msg.
ls_msg-a3 = 'A'.
ls_msg-A3 = 'A'.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
  });

  it("DATA, VALUE", async () => {
    const code = `
DATA foo TYPE i VALUE 10.
WRITE foo.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("10");
  });

  it("DATA, VALUE, structured", async () => {
    const code = `
DATA: BEGIN OF ls_struc,
        c TYPE i VALUE 10,
      END OF ls_struc.
WRITE ls_struc-c.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("10");
  });

  it("DATA, namespaced", async () => {
    const code = `
DATA /foo/bar TYPE i.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
  });

  it.only("DATA: GROUPNAME / INCLUDE TYPE AS", async () => {
    const code = `
TYPES: BEGIN OF ty,
         foo TYPE i,
       END OF ty.
TYPES: BEGIN OF groupname.
         INCLUDE TYPE ty AS gg.
       TYPES END OF groupname.
DATA data TYPE groupname.

data-foo = 1.
WRITE / data-foo.
WRITE / data-gg-foo.

data-gg-foo = 2.
WRITE / data-foo.
WRITE / data-gg-foo.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("1\n1\n2\n2");
  });

});