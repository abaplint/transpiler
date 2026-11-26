import {expect} from "chai";
import {Transpiler} from "../src";
import * as abaplint from "@abaplint/core";
import {IFile, ITranspilerOptions, UnknownTypesEnum} from "../src/types";

async function runFiles(files: IFile[], options?: ITranspilerOptions) {
  const memory = files.map(f => new abaplint.MemoryFile(f.filename, f.contents));
  const reg: abaplint.IRegistry = new abaplint.Registry().addFiles(memory);
  const res = await new Transpiler(options).run(reg);
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


  it("INTF constant from another interface's constant: a literal, no require", async () => {
    const files = [
      {filename: "zif_b.intf.abap", contents: `INTERFACE zif_b PUBLIC.
  CONSTANTS y TYPE string VALUE zif_c=>x.
ENDINTERFACE.`},
      {filename: "zif_c.intf.abap", contents: `INTERFACE zif_c PUBLIC.
  CONSTANTS x TYPE string VALUE 'c'.
ENDINTERFACE.`}];
    const output = await runFiles(files);
    const b = output.find(o => o.filename === "zif_b.intf.mjs")!;
    expect(b.requires).to.deep.equal([]);
    expect(b.chunk.getCode()).to.contain("zif_b.zif_b$y.set('c');");
  });

  it("INTF constant through an interface constant from an unknown class: member name, interface required", async () => {
    const files = [
      {filename: "zif_n.intf.abap", contents: `INTERFACE zif_n PUBLIC.
  CONSTANTS y TYPE string VALUE zif_p=>x.
ENDINTERFACE.`},
      {filename: "zif_p.intf.abap", contents: `INTERFACE zif_p PUBLIC.
  CONSTANTS x TYPE string VALUE cl_void=>z.
ENDINTERFACE.`}];
    // cl_void is unknown, voided as the transpiler does for unknown objects: nothing computes its constant
    const output = await runFiles(files, {unknownTypes: UnknownTypesEnum.runtimeError});
    const n = output.find(o => o.filename === "zif_n.intf.mjs")!;
    expect(n.requires).to.deep.equal([{filename: "zif_p.intf.abap", name: undefined}]);
    expect(n.chunk.getCode()).to.contain("zif_n.zif_n$y.set(abap.Classes['ZIF_P'].zif_p$x);");
  });

  it("INTF constant from a component of a structured constant: not followed, the field chain as before", async () => {
    const files = [
      {filename: "zif_b.intf.abap", contents: `INTERFACE zif_b PUBLIC.
  CONSTANTS y TYPE string VALUE zcl_k=>gs-a.
ENDINTERFACE.`},
      {filename: "zcl_k.clas.abap", contents: `CLASS zcl_k DEFINITION PUBLIC.
  PUBLIC SECTION.
    CONSTANTS: BEGIN OF gs,
                 a TYPE string VALUE 'a',
               END OF gs.
ENDCLASS.
CLASS zcl_k IMPLEMENTATION.
ENDCLASS.`}];
    const output = await runFiles(files);
    const b = output.find(o => o.filename === "zif_b.intf.mjs")!;
    expect(b.requires).to.deep.equal([]);
    expect(b.chunk.getCode()).to.contain("zif_b.zif_b$y.set(abap.Classes['ZCL_K'].gs.get().a);");
  });

});
