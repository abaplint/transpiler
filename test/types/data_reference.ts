import {expect} from "chai";
import {ABAP} from "../../packages/runtime/src/";
import {AsyncFunction, runFiles} from "../_utils";

let abap: ABAP;

async function run(contents: string) {
  return runFiles(abap, [{filename: "zfoobar.prog.abap", contents}]);
}

describe("Running Examples - Data reference", () => {

  beforeEach(async () => {
    abap = new ABAP();
  });

  it("set data reference from data reference", async () => {
    const code = `
TYPES: BEGIN OF node,
         bar TYPE i,
       END OF node.
DATA node TYPE node.
DATA item1 TYPE REF TO node.
DATA item2 TYPE REF TO node.
GET REFERENCE OF node INTO item1.
node-bar = 2.
item2 = item1.
WRITE / item2->bar.

item2->bar = 3.
WRITE / item1->bar.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("2\n3");
  });

  it("clear", async () => {
    const code = `
TYPES: BEGIN OF node,
  bar TYPE i,
END OF node.
DATA node TYPE node.
DATA ref TYPE REF TO node.
DATA tab TYPE STANDARD TABLE OF REF TO node.
node-bar = 2.
GET REFERENCE OF node INTO ref.
APPEND ref TO tab.

CLEAR ref.
READ TABLE tab INDEX 1 INTO ref.
WRITE ref->bar.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("2");
  });

  it("chained set", async () => {
    const code = `
TYPES: BEGIN OF ty,
         descr TYPE string,
       END OF ty.
DATA foo TYPE REF TO ty.
CREATE DATA foo.
foo->*-descr = 'hello'.
WRITE foo->*-descr.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("hello");
  });


});