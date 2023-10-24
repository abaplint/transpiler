import {expect} from "chai";
import {ABAP, MemoryConsole} from "../../packages/runtime/src/";
import {AsyncFunction, runFiles} from "../_utils";

let abap: ABAP;

async function run(contents: string) {
  return runFiles(abap, [{filename: "zfoobar.prog.abap", contents}]);
}

describe("Running Examples - Internal table type", () => {

  beforeEach(async () => {
    abap = new ABAP({console: new MemoryConsole()});
  });

  it("Basic, non sorted", async () => {
    const code = `
    DATA tab TYPE STANDARD TABLE OF i WITH DEFAULT KEY.
    DATA row LIKE LINE OF tab.
    INSERT 1 INTO TABLE tab.
    INSERT 3 INTO TABLE tab.
    INSERT 2 INTO TABLE tab.
    LOOP AT tab INTO row.
      WRITE / row.
    ENDLOOP.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("1\n3\n2");
  });

  it("Basic, sorted", async () => {
    const code = `
  DATA tab TYPE SORTED TABLE OF i WITH UNIQUE KEY table_line.
  DATA row LIKE LINE OF tab.
  INSERT 1 INTO TABLE tab.
  INSERT 3 INTO TABLE tab.
  INSERT 2 INTO TABLE tab.
  LOOP AT tab INTO row.
    WRITE / row.
  ENDLOOP.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("1\n2\n3");
  });

  it("sorted, already exists", async () => {
    const code = `
  DATA tab TYPE SORTED TABLE OF i WITH UNIQUE KEY table_line.
  DATA row LIKE LINE OF tab.
  INSERT 1 INTO TABLE tab.
  WRITE / sy-subrc.
  INSERT 1 INTO TABLE tab.
  WRITE / sy-subrc.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("0\n4");
  });

  it.skip("sorted, duplicate key, expect dump", async () => {
    const code = `
  DATA tab TYPE SORTED TABLE OF i WITH UNIQUE KEY table_line.
  DATA row LIKE LINE OF tab.
  APPEND 1 TO tab.
  APPEND 1 TO tab.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    expect(async () => f(abap)).to.throw();
  });

  it("sorted, structured", async () => {
    const code = `
TYPES: BEGIN OF type,
         foo TYPE i,
         bar TYPE c LENGTH 4,
       END OF type.
DATA tab TYPE SORTED TABLE OF type WITH UNIQUE KEY foo bar.
DATA row LIKE LINE OF tab.
CLEAR row.
row-foo = 1.
row-bar = 'BBBB'.
INSERT row INTO TABLE tab.
CLEAR row.
row-foo = 1.
row-bar = 'AAAA'.
INSERT row INTO TABLE tab.
LOOP AT tab INTO row.
  WRITE / row-bar.
ENDLOOP.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("AAAA\nBBBB");
  });

  it("copying table to sorted table should sort", async () => {
    const code = `
  DATA tab TYPE STANDARD TABLE OF i WITH DEFAULT KEY.
  DATA sorted TYPE SORTED TABLE OF i WITH UNIQUE KEY table_line.
  DATA row LIKE LINE OF tab.
  INSERT 1 INTO TABLE tab.
  INSERT 3 INTO TABLE tab.
  INSERT 2 INTO TABLE tab.
  sorted = tab.
  LOOP AT sorted INTO row.
    WRITE / row.
  ENDLOOP.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("1\n2\n3");
  });

  it("ANY TABLE and data refs", async () => {
    const code = `
TYPES ty TYPE STANDARD TABLE OF i WITH DEFAULT KEY.
DATA tab TYPE ty.
DATA ref TYPE REF TO data.
FIELD-SYMBOLS <tab> TYPE ANY TABLE.
APPEND 2 TO tab.
CREATE DATA ref TYPE ty.
ASSIGN ref->* TO <tab>.
<tab> = tab.
ASSERT lines( <tab> ) = lines( tab ).`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
  });

  it("non-unique key", async () => {
    const code = `
TYPES: BEGIN OF ty_stru,
         field1 TYPE i,
         field2 TYPE i,
       END OF ty_stru.
TYPES ty_tab TYPE SORTED TABLE OF ty_stru WITH NON-UNIQUE KEY field1 field2.
DATA lt_tab TYPE ty_tab.
DATA ls_row TYPE ty_stru.

ls_row-field1 = 1.
ls_row-field2 = 1.
INSERT ls_row INTO TABLE lt_tab.
INSERT ls_row INTO TABLE lt_tab.
ASSERT lines( lt_tab ) = 2.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
  });

  it("table, EQuals", async () => {
    const code = `
DATA tab1 TYPE STANDARD TABLE OF string WITH DEFAULT KEY.
DATA tab2 TYPE STANDARD TABLE OF string WITH DEFAULT KEY.
APPEND 'hello' TO tab1.
APPEND 'hello' TO tab2.
ASSERT tab1 = tab2.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
  });

  it("table, not equals", async () => {
    const code = `
  DATA tab1 TYPE STANDARD TABLE OF string WITH DEFAULT KEY.
  DATA tab2 TYPE STANDARD TABLE OF string WITH DEFAULT KEY.
  APPEND 'hello' TO tab1.
  APPEND 'world' TO tab2.
  ASSERT tab1 <> tab2.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
  });

  it("table, copy full table and perform type conversions", async () => {
    const code = `
    DATA strings TYPE STANDARD TABLE OF string WITH DEFAULT KEY.
    DATA ints TYPE STANDARD TABLE OF i WITH DEFAULT KEY.
    DATA lv_type TYPE c LENGTH 1.
    FIELD-SYMBOLS <fs> TYPE any.
    APPEND '2' TO strings.
    ints = strings. " type conversion
    READ TABLE ints INDEX 1 ASSIGNING <fs>.
    DESCRIBE FIELD <fs> TYPE lv_type.
    WRITE lv_type.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("I");
  });

  it("copy, different column names1", async () => {
    const code = `
TYPES: BEGIN OF type1,
         foo TYPE i,
       END OF type1.
TYPES: BEGIN OF type2,
         bar TYPE i,
       END OF type2.
DATA tab1 TYPE STANDARD TABLE OF type1 WITH DEFAULT KEY.
DATA tab2 TYPE STANDARD TABLE OF type2 WITH DEFAULT KEY.
APPEND INITIAL LINE TO tab2.
tab1 = tab2.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
  });

  it("copy, different column names2", async () => {
    const code = `
TYPES: BEGIN OF alphatab_type,
         cola TYPE string,
       END OF alphatab_type.
TYPES alphas TYPE STANDARD TABLE OF alphatab_type.
TYPES: BEGIN OF combined_data_type,
         colx TYPE string,
       END OF combined_data_type.
TYPES combined_data TYPE STANDARD TABLE OF combined_data_type.
DATA alphas         TYPE alphas.
DATA combined_data  TYPE combined_data.
APPEND INITIAL LINE TO alphas.
combined_data[] = alphas[].`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
  });

  it("copy, standard table into hashed", async () => {
    const code = `
TYPES:
  BEGIN OF ty_s_data,
    key1 TYPE string,
  END OF ty_s_data.

DATA: lt_table1  TYPE STANDARD TABLE OF ty_s_data,
      lt_table6  TYPE HASHED TABLE OF ty_s_data WITH UNIQUE KEY table_line,
      ls_table   TYPE ty_s_data,
      lv_counter TYPE n LENGTH 2.

DO 2 TIMES.
  CONCATENATE 'k1' lv_counter INTO ls_table-key1.
  APPEND ls_table TO lt_table1.
  lv_counter = lv_counter + 1.
ENDDO.

lt_table6 = lt_table1.
ASSERT lines( lt_table6 ) = 2.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
  });

  it("copy, sorted table line, deep key", async () => {
    const code = `
TYPES: BEGIN OF ty_item,
         obj_type TYPE string,
         obj_name TYPE string,
       END OF ty_item.
TYPES: BEGIN OF ty,
         item TYPE ty_item,
       END OF ty.
DATA row TYPE ty.
DATA normal TYPE STANDARD TABLE OF ty WITH DEFAULT KEY.
DATA sorted TYPE SORTED TABLE OF ty WITH NON-UNIQUE KEY item-obj_type item-obj_name.

row-item-obj_type = 'B'.
APPEND row TO normal.
row-item-obj_type = 'A'.
APPEND row TO normal.

sorted = normal.

LOOP AT sorted INTO row.
  WRITE / row-item-obj_type.
ENDLOOP.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("A\nB");
  });

  it("hashed table with table_line key", async () => {
    const code = `
DATA tab TYPE HASHED TABLE OF string WITH UNIQUE KEY table_line.
DATA row LIKE LINE OF tab.

row = 'CC'.
INSERT row INTO TABLE tab.
row = 'CC'.
INSERT row INTO TABLE tab.
row = 'AA'.
INSERT row INTO TABLE tab.
row = 'BB'.
INSERT row INTO TABLE tab.

ASSERT lines( tab ) = 3.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
  });

  it("compare hashed tables", async () => {
    const code = `
DATA tab1 TYPE HASHED TABLE OF string WITH UNIQUE KEY table_line.
DATA tab2 TYPE HASHED TABLE OF string WITH UNIQUE KEY table_line.
ASSERT tab1 = tab2.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
  });

  it("HASHED and AS, found", async () => {
    const code = `
TYPES:
  BEGIN OF ty_instance_key,
    val1 TYPE c LENGTH 4,
    val2 TYPE n LENGTH 2,
  END OF ty_instance_key .
TYPES:
  BEGIN OF ty_instance.
    INCLUDE TYPE ty_instance_key AS key.
    TYPES: instance TYPE i,
  END OF ty_instance.
TYPES ty_t_instances TYPE HASHED TABLE OF ty_instance WITH UNIQUE KEY key.
DATA gt_instances TYPE ty_t_instances.
DATA ls_instance TYPE ty_instance.

ls_instance-key-val1 = 'FOOB'.
ls_instance-key-val2 = '42'.
ls_instance-instance = 5.
INSERT ls_instance INTO TABLE gt_instances.
CLEAR ls_instance-instance.

READ TABLE gt_instances
  WITH TABLE KEY key = ls_instance-key
  INTO ls_instance
  TRANSPORTING instance.
WRITE / sy-subrc.
WRITE / ls_instance-instance.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("0\n5");
  });

  it("HASHED and AS, not found", async () => {
    const code = `
TYPES:
  BEGIN OF ty_instance_key,
    val1 TYPE c LENGTH 4,
    val2 TYPE n LENGTH 2,
  END OF ty_instance_key .
TYPES:
  BEGIN OF ty_instance.
    INCLUDE TYPE ty_instance_key AS key.
    TYPES: instance TYPE i,
  END OF ty_instance.
TYPES ty_t_instances TYPE HASHED TABLE OF ty_instance WITH UNIQUE KEY key.
DATA gt_instances TYPE ty_t_instances.
DATA ls_instance TYPE ty_instance.

ls_instance-key-val1 = 'FOOB'.
ls_instance-key-val2 = '42'.
ls_instance-instance = 5.
INSERT ls_instance INTO TABLE gt_instances.
CLEAR ls_instance-instance.

ls_instance-key-val1 = 'NOOO'.

READ TABLE gt_instances
  WITH TABLE KEY key = ls_instance-key
  INTO ls_instance
  TRANSPORTING instance.
WRITE / sy-subrc.
WRITE / ls_instance-instance.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("4\n0");
  });

  it("hashed table, sequence", async () => {
    const code = `
TYPES: BEGIN OF ty,
         field1 TYPE c LENGTH 2,
         field2 TYPE i,
       END OF ty.
DATA tab TYPE HASHED TABLE OF ty WITH UNIQUE KEY field1.
DATA row LIKE LINE OF tab.

row-field1 = 'CC'.
INSERT row INTO TABLE tab.
row-field1 = 'CC'.
INSERT row INTO TABLE tab.
row-field1 = 'AA'.
INSERT row INTO TABLE tab.
row-field1 = 'BB'.
INSERT row INTO TABLE tab.

ASSERT lines( tab ) = 3.

LOOP AT tab INTO row.
  WRITE / row-field1.
ENDLOOP.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("CC\nAA\nBB");
  });

  it("hashed table, nested structured key", async () => {
    const code = `
TYPES: BEGIN OF ty,
         bar TYPE c LENGTH 1,
         BEGIN OF sub,
           foo TYPE c LENGTH 1,
           BEGIN OF subsub,
             foo TYPE c LENGTH 1,
           END OF subsub,
         END OF sub,
       END OF ty.
DATA tab TYPE HASHED TABLE OF ty WITH UNIQUE KEY sub.
DATA data1 TYPE ty.
DATA data2 TYPE ty.

data1-sub-subsub-foo = 'A'.
data2-sub-subsub-foo = 'B'.
ASSERT data1 <> data2.

INSERT data1 INTO TABLE tab.
ASSERT sy-subrc = 0.

INSERT data2 INTO TABLE tab.
ASSERT sy-subrc = 0.

WRITE / lines( tab ).`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("2");
  });

  it("set hashed table", async () => {
    const code = `
DATA tab1 TYPE HASHED TABLE OF i WITH UNIQUE KEY table_line.
DATA tab2 TYPE HASHED TABLE OF i WITH UNIQUE KEY table_line.
INSERT 1 INTO TABLE tab1.
INSERT 2 INTO TABLE tab1.
INSERT 3 INTO TABLE tab2.
tab2 = tab1.
ASSERT lines( tab2 ) = 2.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
  });

  it("set hashed table, with field symbols", async () => {
    const code = `
DATA tab1 TYPE HASHED TABLE OF i WITH UNIQUE KEY table_line.
DATA tab2 TYPE HASHED TABLE OF i WITH UNIQUE KEY table_line.
FIELD-SYMBOLS <fs1> TYPE ANY TABLE.
FIELD-SYMBOLS <fs2> TYPE ANY TABLE.
INSERT 1 INTO TABLE tab1.
INSERT 2 INTO TABLE tab1.
INSERT 3 INTO TABLE tab2.
ASSIGN tab1 TO <fs1>.
ASSIGN tab2 TO <fs2>.
<fs2> = <fs1>.
ASSERT lines( <fs2> ) = 2.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
  });

  it("hashed table, DELETE TABLE FROM", async () => {
    const code = `
DATA tab1 TYPE HASHED TABLE OF i WITH UNIQUE KEY table_line.
DATA row LIKE LINE OF tab1.
row = 2.
INSERT row INTO TABLE tab1.
DELETE TABLE tab1 FROM row.
WRITE lines( tab1 ).`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
  });

  it("self move", async () => {
    const code = `
TYPES ty TYPE STANDARD TABLE OF string WITH DEFAULT KEY.
DATA dat TYPE ty.
APPEND 'sdfsdfsdf' TO dat.
dat = dat.
ASSERT lines( dat ) = 1.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
  });

});