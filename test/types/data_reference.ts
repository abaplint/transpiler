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

  it.skip("data references and field symbols", async () => {
    const code = `
DATA int TYPE i.
DATA ref1 TYPE REF TO data.
DATA ref2 TYPE REF TO data.
FIELD-SYMBOLS <fs> TYPE data.
FIELD-SYMBOLS <any> TYPE any.

GET REFERENCE OF int INTO ref1.
ASSIGN ref1 TO <fs>.

ref2 = <fs>.

ASSIGN ref2->* TO <any>.
<any> = 2.

WRITE int.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("2");
  });

  it("more data references and field symbols", async () => {
    const code = `
DATA ref TYPE REF TO i.
FIELD-SYMBOLS <ref> TYPE data.
FIELD-SYMBOLS <data> TYPE data.
CREATE DATA ref.
ASSIGN ref TO <ref>.
ASSIGN <ref>->* TO <data>.
ref->* = 2.
WRITE <data>.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("2");
  });

  it("another data references and field symbols", async () => {
    const code = `
DATA ref TYPE REF TO i.
FIELD-SYMBOLS <ref> TYPE data.
FIELD-SYMBOLS <data> TYPE data.
CREATE DATA ref.
ASSIGN ref TO <ref>.
ASSIGN <ref>->* TO <data>.
<data> = 2.
WRITE <data>.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("2");
  });

  it("type after assign", async () => {
    const code = `
DATA lv_typ TYPE c LENGTH 1.
DATA ref TYPE REF TO i.
FIELD-SYMBOLS <ref> TYPE data.
FIELD-SYMBOLS <data> TYPE data.
CREATE DATA ref.
ASSIGN ref TO <ref>.
ASSIGN <ref>->* TO <data>.
DESCRIBE FIELD <data> TYPE lv_typ.
WRITE lv_typ.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("I");
  });

  it("unassigned", async () => {
    const code = `
DATA ref TYPE REF TO i.
FIELD-SYMBOLS <fs> TYPE data.
ref = <fs>.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    try {
      await f(abap);
      expect.fail();
    } catch(e) {
      expect(e).to.equal("GETWA_NOT_ASSIGNED");
    }
  });

  it("invalid assignment", async () => {
    const code = `
DATA int TYPE i.
DATA ref TYPE REF TO i.
FIELD-SYMBOLS <fs> TYPE data.
ASSIGN int TO <fs>.
ref = <fs>.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    try {
      await f(abap);
      expect.fail();
    } catch(e) {
      expect(e).to.equal("OBJECTS_MOVE_NOT_SUPPORTED");
    }
  });

});