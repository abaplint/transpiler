import {expect} from "chai";
import {Transpiler} from "../src";
import * as abaplint from "@abaplint/core";
import {IFile} from "../src/types";

async function runFiles(files: IFile[]) {
  const memory = files.map(f => new abaplint.MemoryFile(f.filename, f.contents));
  const reg: abaplint.IRegistry = new abaplint.Registry().addFiles(memory).parse();
  const res = await new Transpiler().run(reg);
  return res.objects;
}


describe("Requires", () => {

  it("CLAS using CLAS, 1", async () => {
    const clas1 = `
CLASS zcl_foo DEFINITION PUBLIC FINAL CREATE PUBLIC.
  PUBLIC SECTION.
    METHODS: run.
ENDCLASS.
CLASS zcl_foo IMPLEMENTATION.
  METHOD run.
    DATA bar TYPE REF TO zcl_bar.
    CREATE OBJECT bar.
  ENDMETHOD.
ENDCLASS.`;

    const clas2 = `
CLASS zcl_bar DEFINITION PUBLIC FINAL CREATE PUBLIC.
  PUBLIC SECTION.
ENDCLASS.
CLASS zcl_bar IMPLEMENTATION.
ENDCLASS.`;

    const files = [
      {filename: "zcl_foo.clas.abap", contents: clas1},
      {filename: "zcl_bar.clas.abap", contents: clas2}];

    const output = await runFiles(files);
    expect(output.length).to.equal(2);
    expect(output[0].chunk.getCode()).to.contain("class zcl_foo ");
    expect(output[1].chunk.getCode()).to.contain("class zcl_bar ");

    expect(output[0].requires.length).to.equal(0);
  });

  it("CLAS using CLAS, static reference", async () => {
    const clas1 = `
CLASS zcl_foo DEFINITION PUBLIC FINAL CREATE PUBLIC.
  PUBLIC SECTION.
    METHODS: run.
ENDCLASS.
CLASS zcl_foo IMPLEMENTATION.
  METHOD run.
    cl_abap_unit_assert=>assert_equals( act = 'Y' exp = 'X' ).
    cl_abap_unit_assert=>assert_equals( act = 'Y' exp = 'X' ).
  ENDMETHOD.
ENDCLASS.`;

    const clas2 = `
CLASS cl_abap_unit_assert DEFINITION PUBLIC.
  PUBLIC SECTION.
    CLASS-METHODS
      assert_equals
        IMPORTING
          act TYPE string
          exp TYPE string.
ENDCLASS.

CLASS cl_abap_unit_assert IMPLEMENTATION.
  METHOD assert_equals.
    ASSERT act = exp.
  ENDMETHOD.
ENDCLASS.`;

    const files = [
      {filename: "zcl_foo.clas.abap", contents: clas1},
      {filename: "cl_abap_unit_assert.clas.abap", contents: clas2}];

    const output = await runFiles(files);
    expect(output.length).to.equal(2);
    expect(output[0].chunk.getCode()).to.contain("class zcl_foo ");
    expect(output[1].chunk.getCode()).to.contain("class cl_abap_unit_assert ");

    expect(output[0].requires.length).to.equal(0, "expected one require");
  });

  it("Dont require itself", async () => {
    const clas1 = `
CLASS zcl_foo DEFINITION PUBLIC FINAL CREATE PUBLIC.
  PUBLIC SECTION.
    METHODS: run.
ENDCLASS.
CLASS zcl_foo IMPLEMENTATION.
  METHOD run.
    DATA bar TYPE REF TO zcl_foo.
    CREATE OBJECT bar.
  ENDMETHOD.
ENDCLASS.`;

    const files = [{filename: "zcl_foo.clas.abap", contents: clas1}];

    const output = await runFiles(files);
    expect(output.length).to.equal(1);
    expect(output[0].chunk.getCode()).to.contain("class zcl_foo ");
    expect(output[0].requires.length).to.equal(0, "expected zero requires");
  });

  it("clas, require locals", async () => {
    const clas1 = `
  CLASS zcl_locals DEFINITION PUBLIC FINAL CREATE PUBLIC.
    PUBLIC SECTION.
      CLASS-METHODS sdfsd .
    PROTECTED SECTION.
    PRIVATE SECTION.
  ENDCLASS.
  CLASS ZCL_LOCALS IMPLEMENTATION.
    METHOD sdfsd.
      DATA foo TYPE REF TO lcl_helper.
      CREATE OBJECT foo.
    ENDMETHOD.
  ENDCLASS.`;

    const locals = `
    CLASS lcl_helper DEFINITION.
    ENDCLASS.

    CLASS lcl_helper IMPLEMENTATION.
    ENDCLASS.`;

    const files = [
      {filename: "zcl_locals.clas.abap", contents: clas1},
      {filename: "zcl_locals.clas.locals_imp.abap", contents: locals},
    ];

    const output = await runFiles(files);
    expect(output.length).to.equal(2);
    expect(output[0].chunk.getCode()).to.contain("class zcl_locals ");
    expect(output[0].requires.length).to.equal(1, "expected local require");
  });

});
