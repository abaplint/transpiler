import {expect} from "chai";
import {ABAP} from "../../packages/runtime/src";
import {AsyncFunction, runFiles} from "../_utils";

let abap: ABAP;

async function run(contents: string) {
  return runFiles(abap, [{filename: "zgetreference.prog.abap", contents}]);
}

describe("Running statements - GET REFERENCE", () => {

  beforeEach(async () => {
    abap = new ABAP();
  });

  it("GET REFERENCE", async () => {
    const code = `
TYPES: BEGIN OF ty_structure,
         field TYPE string,
       END OF ty_structure.
DATA ls_data TYPE ty_structure.
DATA ref TYPE REF TO ty_structure.
FIELD-SYMBOLS <fs> TYPE ty_structure.
ls_data-field = 'bar'.
ASSIGN ls_data TO <fs>.
GET REFERENCE OF <fs> INTO ref.
WRITE ref->field.`;

    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("bar");
  });

  it("data reference, dereference", async () => {
    const code = `
  DATA int TYPE i VALUE 2.
  DATA ref TYPE REF TO i.
  GET REFERENCE OF int INTO ref.
  WRITE ref->*.`;

    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("2");
  });

  it("GET REFERENCE, 2", async () => {
    const code = `
TYPES: BEGIN OF ty_structure,
         field TYPE string,
       END OF ty_structure.
DATA ls_data TYPE ty_structure.
DATA ref TYPE REF TO ty_structure.
DATA tab TYPE STANDARD TABLE OF REF TO ty_structure WITH DEFAULT KEY..
FIELD-SYMBOLS <row> LIKE LINE OF tab.
ls_data-field = 'bar'.
GET REFERENCE OF ls_data INTO ref.
APPEND ref TO tab.
LOOP AT tab ASSIGNING <row>.
  WRITE <row>->field.
ENDLOOP.`;

    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("bar");
  });

  it("GET REFERENCE, 3", async () => {
    const code = `
TYPES: BEGIN OF ty_structure,
         field TYPE string,
       END OF ty_structure.
DATA ls_data TYPE ty_structure.
DATA ref TYPE REF TO ty_structure.
DATA tab TYPE STANDARD TABLE OF REF TO ty_structure WITH DEFAULT KEY..
FIELD-SYMBOLS <row> LIKE LINE OF tab.
ls_data-field = 'before'.
GET REFERENCE OF ls_data INTO ref.
APPEND ref TO tab.
ls_data-field = 'after'.
LOOP AT tab ASSIGNING <row>.
  WRITE <row>->field.
ENDLOOP.`;

    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("after");
  });

  it("data references, loop", async () => {
    const code = `
TYPES: BEGIN OF ty_node,
         children TYPE i,
       END OF ty_node.
TYPES ty_stack_tt TYPE STANDARD TABLE OF REF TO ty_node.
DATA rt_tree TYPE STANDARD TABLE OF ty_node WITH DEFAULT KEY.
DATA mt_stack TYPE ty_stack_tt.
DATA lr_stack_top LIKE LINE OF mt_stack.
FIELD-SYMBOLS <item> LIKE LINE OF rt_tree.

APPEND INITIAL LINE TO rt_tree ASSIGNING <item>.
GET REFERENCE OF <item> INTO lr_stack_top.
INSERT lr_stack_top INTO mt_stack INDEX 1.

READ TABLE mt_stack INDEX 1 INTO lr_stack_top.
ASSERT sy-subrc = 0.
lr_stack_top->children = lr_stack_top->children + 1.

LOOP AT mt_stack INTO lr_stack_top.
  WRITE lr_stack_top->children.
ENDLOOP.`;

    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("1");
  });

  it("data references, field symbols", async () => {
    const code = `
TYPES: BEGIN OF ty_node,
         children TYPE i,
       END OF ty_node.
TYPES ty_stack_tt TYPE STANDARD TABLE OF REF TO ty_node.
DATA rt_tree TYPE STANDARD TABLE OF ty_node WITH DEFAULT KEY.
DATA mt_stack TYPE ty_stack_tt.
DATA lr_stack_top LIKE LINE OF mt_stack.
FIELD-SYMBOLS <item> LIKE LINE OF rt_tree.

APPEND INITIAL LINE TO rt_tree ASSIGNING <item>.
<item>-children = 33.
GET REFERENCE OF <item> INTO lr_stack_top.
lr_stack_top->children = 22.
INSERT lr_stack_top INTO mt_stack INDEX 1.
lr_stack_top->children = 44.

READ TABLE mt_stack INDEX 1 INTO lr_stack_top.
ASSERT sy-subrc = 0.
lr_stack_top->children = 1.

WRITE <item>-children.`;

    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("1");
  });

});