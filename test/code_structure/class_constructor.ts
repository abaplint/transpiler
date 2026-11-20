import {expect} from "chai";
import {ABAP, MemoryConsole} from "../../packages/runtime/src";
import {AsyncFunction, runFiles} from "../_utils";

let abap: ABAP;

async function run(contents: string) {
  return runFiles(abap, [{filename: "zfoobar.prog.abap", contents}]);
}

async function execute(code: string): Promise<string> {
  const js = await run(code);
  const f = new AsyncFunction("abap", js);
  await f(abap);
  return abap.console.get();
}

// when a class constructor runs, the expected output of every test is the output of the same program on a 7.58 system
describe("Running code structure - class constructor timing", () => {

  beforeEach(async () => {
    abap = new ABAP({console: new MemoryConsole()});
  });

  it("static method call: at the statement, not when the program is loaded", async () => {
    const out = await execute(`
CLASS lcl DEFINITION.
  PUBLIC SECTION.
    CLASS-METHODS class_constructor.
    CLASS-METHODS touch.
ENDCLASS.
CLASS lcl IMPLEMENTATION.
  METHOD class_constructor.
    WRITE / 'ctor'.
  ENDMETHOD.
  METHOD touch.
    WRITE / 'touch'.
  ENDMETHOD.
ENDCLASS.

START-OF-SELECTION.
WRITE / 'before'.
lcl=>touch( ).
WRITE / 'after'.`);
    expect(out).to.equal("before\nctor\ntouch\nafter");
  });

  it("the class constructor runs inside the loop, its APPEND sets sy-tabix", async () => {
    const out = await execute(`
CLASS lcl DEFINITION.
  PUBLIC SECTION.
    CLASS-DATA gt TYPE STANDARD TABLE OF string WITH EMPTY KEY.
    CLASS-METHODS class_constructor.
    CLASS-METHODS touch.
ENDCLASS.
CLASS lcl IMPLEMENTATION.
  METHOD class_constructor.
    APPEND 'p' TO gt.
    APPEND 'q' TO gt.
    APPEND 'r' TO gt.
  ENDMETHOD.
  METHOD touch.
  ENDMETHOD.
ENDCLASS.

START-OF-SELECTION.
DATA lt TYPE STANDARD TABLE OF string WITH EMPTY KEY.
DATA lv_out TYPE string.
APPEND 'a' TO lt.
APPEND 'b' TO lt.
LOOP AT lt INTO DATA(lv).
  lcl=>touch( ).
  lv_out = lv_out && |{ sy-tabix }|.
ENDLOOP.
WRITE / lv_out.`);
    expect(out).to.equal("32");
  });

  it("static method call in a FORM, in a branch not taken, dynamic call", async () => {
    const out = await execute(`
CLASS lcl DEFINITION.
  PUBLIC SECTION.
    CLASS-METHODS class_constructor.
    CLASS-METHODS touch.
ENDCLASS.
CLASS lcl IMPLEMENTATION.
  METHOD class_constructor.
    WRITE / 'ctor'.
  ENDMETHOD.
  METHOD touch.
    WRITE / 'touch'.
  ENDMETHOD.
ENDCLASS.

CLASS lcl_dyn DEFINITION.
  PUBLIC SECTION.
    CLASS-METHODS class_constructor.
    CLASS-METHODS touch.
ENDCLASS.
CLASS lcl_dyn IMPLEMENTATION.
  METHOD class_constructor.
    WRITE / 'dyn ctor'.
  ENDMETHOD.
  METHOD touch.
    WRITE / 'dyn touch'.
  ENDMETHOD.
ENDCLASS.

CLASS lcl_never DEFINITION.
  PUBLIC SECTION.
    CLASS-METHODS class_constructor.
    CLASS-METHODS touch.
ENDCLASS.
CLASS lcl_never IMPLEMENTATION.
  METHOD class_constructor.
    WRITE / 'never ctor'.
  ENDMETHOD.
  METHOD touch.
  ENDMETHOD.
ENDCLASS.

FORM use.
  WRITE / 'form entry'.
  lcl=>touch( ).
ENDFORM.

FORM unused_branch.
  IF 1 = 2.
    lcl_never=>touch( ).
  ENDIF.
ENDFORM.

START-OF-SELECTION.
  DATA name TYPE string.
  WRITE / 'before'.
  PERFORM use.
  PERFORM unused_branch.
  WRITE / 'mid'.
  name = 'LCL_DYN'.
  CALL METHOD (name)=>('TOUCH').
  WRITE / 'after'.`);
    expect(out).to.equal("before\nform entry\nctor\ntouch\nmid\ndyn ctor\ndyn touch\nafter");
  });

  it("static attribute: on entry of the procedure, also from a branch not taken; NEW at the statement", async () => {
    const out = await execute(`
CLASS lcl_a DEFINITION.
  PUBLIC SECTION.
    CLASS-DATA v TYPE string.
    CLASS-METHODS class_constructor.
ENDCLASS.
CLASS lcl_a IMPLEMENTATION.
  METHOD class_constructor.
    WRITE / 'a ctor'.
  ENDMETHOD.
ENDCLASS.

CLASS lcl_b DEFINITION.
  PUBLIC SECTION.
    CLASS-DATA v TYPE string.
    CLASS-METHODS class_constructor.
ENDCLASS.
CLASS lcl_b IMPLEMENTATION.
  METHOD class_constructor.
    WRITE / 'b ctor'.
  ENDMETHOD.
ENDCLASS.

CLASS lcl_c DEFINITION.
  PUBLIC SECTION.
    CLASS-DATA v TYPE string.
    CLASS-METHODS class_constructor.
ENDCLASS.
CLASS lcl_c IMPLEMENTATION.
  METHOD class_constructor.
    WRITE / 'c ctor'.
  ENDMETHOD.
ENDCLASS.

CLASS lcl_d DEFINITION.
  PUBLIC SECTION.
    CLASS-METHODS class_constructor.
    METHODS m.
ENDCLASS.
CLASS lcl_d IMPLEMENTATION.
  METHOD class_constructor.
    WRITE / 'd ctor'.
  ENDMETHOD.
  METHOD m.
  ENDMETHOD.
ENDCLASS.

FORM read_attr.
  DATA x TYPE string.
  WRITE / 'read_attr entry'.
  x = lcl_a=>v.
  WRITE / 'read_attr end'.
ENDFORM.

FORM untaken_attr.
  DATA x TYPE string.
  WRITE / 'untaken entry'.
  IF 1 = 2.
    x = lcl_b=>v.
  ENDIF.
  WRITE / 'untaken end'.
ENDFORM.

FORM write_attr.
  WRITE / 'write_attr entry'.
  lcl_c=>v = 'x'.
ENDFORM.

FORM new_obj.
  DATA o TYPE REF TO lcl_d.
  WRITE / 'new_obj entry'.
  o = NEW #( ).
ENDFORM.

START-OF-SELECTION.
  WRITE / 'start'.
  PERFORM read_attr.
  PERFORM untaken_attr.
  PERFORM write_attr.
  PERFORM new_obj.
  WRITE / 'end'.`);
    expect(out).to.equal("start\na ctor\nread_attr entry\nread_attr end\nb ctor\nuntaken entry\nuntaken end\n"
      + "c ctor\nwrite_attr entry\nnew_obj entry\nd ctor\nend");
  });

  it("inherited static components through a sub class name run only the declaring class constructor", async () => {
    const out = await execute(`
CLASS lcl_p DEFINITION.
  PUBLIC SECTION.
    CLASS-DATA v TYPE string.
    CLASS-METHODS class_constructor.
    CLASS-METHODS get RETURNING VALUE(r) TYPE string.
ENDCLASS.
CLASS lcl_p IMPLEMENTATION.
  METHOD class_constructor.
    v = v && 'P'.
  ENDMETHOD.
  METHOD get.
    r = v.
  ENDMETHOD.
ENDCLASS.
CLASS lcl_s1 DEFINITION INHERITING FROM lcl_p.
  PUBLIC SECTION.
    CLASS-METHODS class_constructor.
ENDCLASS.
CLASS lcl_s1 IMPLEMENTATION.
  METHOD class_constructor.
    v = v && '1'.
  ENDMETHOD.
ENDCLASS.
CLASS lcl_s5 DEFINITION INHERITING FROM lcl_p.
  PUBLIC SECTION.
    CLASS-DATA w TYPE string.
    CLASS-METHODS class_constructor.
ENDCLASS.
CLASS lcl_s5 IMPLEMENTATION.
  METHOD class_constructor.
    v = v && '5'.
  ENDMETHOD.
ENDCLASS.

FORM a.
  DATA x TYPE string.
  x = lcl_s1=>v.
  WRITE / |a { x }|.
  x = lcl_s1=>get( ).
  WRITE / |b { x }|.
ENDFORM.
FORM c.
  DATA x TYPE string.
  x = lcl_s5=>v.
  WRITE / |c { x }|.
ENDFORM.

START-OF-SELECTION.
  PERFORM a.
  PERFORM c.`);
    expect(out).to.equal("a P\nb P\nc P");
  });

  it("in a method of a class: the own and super class constructors have run, a sub class is checked on entry", async () => {
    const out = await execute(`
CLASS lcl_s DEFINITION DEFERRED.
CLASS lcl_p DEFINITION.
  PUBLIC SECTION.
    CLASS-DATA v TYPE string.
    CLASS-METHODS class_constructor.
    METHODS read_sub.
    METHODS read_own.
ENDCLASS.
CLASS lcl_s DEFINITION INHERITING FROM lcl_p.
  PUBLIC SECTION.
    CLASS-DATA w TYPE string.
    CLASS-METHODS class_constructor.
    METHODS read_super.
ENDCLASS.
CLASS lcl_p IMPLEMENTATION.
  METHOD class_constructor.
    WRITE / 'p ctor'.
  ENDMETHOD.
  METHOD read_sub.
    DATA x TYPE string.
    WRITE / 'read_sub entry'.
    x = lcl_s=>w.
  ENDMETHOD.
  METHOD read_own.
    DATA x TYPE string.
    WRITE / 'read_own entry'.
    x = lcl_p=>v.
  ENDMETHOD.
ENDCLASS.
CLASS lcl_s IMPLEMENTATION.
  METHOD class_constructor.
    WRITE / 's ctor'.
  ENDMETHOD.
  METHOD read_super.
    DATA x TYPE string.
    WRITE / 'read_super entry'.
    x = lcl_p=>v.
  ENDMETHOD.
ENDCLASS.

START-OF-SELECTION.
  DATA p TYPE REF TO lcl_p.
  DATA s TYPE REF TO lcl_s.
  WRITE / 'start'.
  p = NEW #( ).
  p->read_own( ).
  p->read_sub( ).
  s = NEW #( ).
  s->read_super( ).
  WRITE / 'end'.`);
    expect(out).to.equal("start\np ctor\nread_own entry\ns ctor\nread_sub entry\nread_super entry\nend");
  });

});
