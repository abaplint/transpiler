import {expect} from "chai";
import {Transpiler} from "../src";

describe("Requires", () => {

  it("CLAS using CLAS", async () => {
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

    const output = (await new Transpiler().run(files)).objects;
    expect(output.length).to.equal(2);
    expect(output[0].js.contents).to.contain("class zcl_foo ");
    expect(output[1].js.contents).to.contain("class zcl_bar ");

    expect(output[0].requires.length).to.equal(1, "expected one require");
    expect(output[0].requires[0].type).to.equal("CLAS");
    expect(output[0].requires[0].name).to.equal("ZCL_BAR");
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

    const output = (await new Transpiler().run(files)).objects;
    expect(output.length).to.equal(2);
    expect(output[0].js.contents).to.contain("class zcl_foo ");
    expect(output[1].js.contents).to.contain("class cl_abap_unit_assert ");

    expect(output[0].requires.length).to.equal(1, "expected one require");
    expect(output[0].requires[0].type).to.equal("CLAS");
    expect(output[0].requires[0].name).to.equal("CL_ABAP_UNIT_ASSERT");
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

    const output = (await new Transpiler().run(files)).objects;
    expect(output.length).to.equal(1);
    expect(output[0].js.contents).to.contain("class zcl_foo ");

    expect(output[0].requires.length).to.equal(0, "expected zero requires");
  });

});
