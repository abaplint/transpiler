import {expect} from "chai";
import {ABAP, MemoryConsole} from "../../packages/runtime/src";
import {AsyncFunction, runFiles} from "../_utils";

let abap: ABAP;

async function run(contents: string) {
  return runFiles(abap, [{filename: "zfoobar.prog.abap", contents}]);
}

describe("Running statements - CASE TYPE", () => {

  beforeEach(async () => {
    abap = new ABAP({console: new MemoryConsole()});
  });

  it("CASE TYPE, basic", async () => {
    const code = `
CLASS lcl DEFINITION.
  PUBLIC SECTION.
    DATA foo TYPE string.
ENDCLASS.
CLASS lcl IMPLEMENTATION.
ENDCLASS.

START-OF-SELECTION.
  DATA lo_artefact TYPE REF TO object.
  DATA lo_lcl TYPE REF TO lcl.
  CREATE OBJECT lo_artefact TYPE lcl.
  CASE TYPE OF lo_artefact.
    WHEN TYPE lcl INTO lo_lcl.
      WRITE lo_lcl->foo.
      WRITE 'world'.
    WHEN OTHERS.
      WRITE 'hello'.
  ENDCASE.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("world");
  });

  it("CASE TYPE, WHEN TYPE without INTO, interfaces and initial references", async () => {
    const code = `
INTERFACE lif.
  METHODS name RETURNING VALUE(r) TYPE string.
ENDINTERFACE.
CLASS lcl_a DEFINITION.
  PUBLIC SECTION.
    INTERFACES lif.
ENDCLASS.
CLASS lcl_a IMPLEMENTATION.
  METHOD lif~name.
    r = 'A'.
  ENDMETHOD.
ENDCLASS.
CLASS lcl_b DEFINITION INHERITING FROM lcl_a.
  PUBLIC SECTION.
    METHODS lif~name REDEFINITION.
ENDCLASS.
CLASS lcl_b IMPLEMENTATION.
  METHOD lif~name.
    r = 'B'.
  ENDMETHOD.
ENDCLASS.
CLASS lcl_c DEFINITION.
ENDCLASS.
CLASS lcl_c IMPLEMENTATION.
ENDCLASS.

START-OF-SELECTION.
  DATA obj TYPE REF TO object.
  DATA i TYPE REF TO lif.
  DATA initial_b TYPE REF TO lcl_b.
  DATA res TYPE string.

  obj = NEW lcl_b( ).
  CASE TYPE OF obj.
    WHEN TYPE lcl_a.
      res = res && 'A'.
    WHEN TYPE lcl_b.
      res = res && 'B'.
  ENDCASE.
  res = res && ','.

  CASE TYPE OF obj.
    WHEN TYPE lcl_c INTO DATA(c).
      res = res && 'C'.
    WHEN TYPE lif INTO i.
      res = res && 'I' && i->name( ).
    WHEN OTHERS.
      res = res && 'O'.
  ENDCASE.
  res = res && ','.

  obj = NEW lcl_c( ).
  CASE TYPE OF obj.
    WHEN TYPE lif.
      res = res && 'I'.
    WHEN TYPE lcl_c.
      res = res && 'C'.
  ENDCASE.
  res = res && ','.

  CASE TYPE OF initial_b.
    WHEN TYPE lif.
      res = res && 'I'.
    WHEN OTHERS.
      res = res && 'O'.
  ENDCASE.
  res = res && ','.

  CLEAR obj.
  CASE TYPE OF obj.
    WHEN TYPE lcl_a.
      res = res && 'A'.
    WHEN OTHERS.
      res = res && 'O'.
  ENDCASE.
  ASSERT res = 'A,IB,C,I,O'.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
  });

});