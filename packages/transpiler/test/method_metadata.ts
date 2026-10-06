/* eslint-disable max-len */
import {expect} from "chai";
import {runSingle} from "./_utils";
import {UnknownTypesEnum} from "../src/types";

// the METHODS metadata of the class or interface with the given name, as an object
async function methodsOf(abap: string, name: string): Promise<any> {
  // the exception classes of the RAISING clauses are not part of the snippets
  const js = await runSingle(abap, {unknownTypes: UnknownTypesEnum.runtimeError});
  const start = js!.indexOf(`class ${name} `);
  expect(start).to.be.greaterThan(-1);
  const from = js!.indexOf("static METHODS = ", start) + "static METHODS = ".length;
  const to = js!.indexOf("};\n", from) + 1;
  // the type factories reference the runtime, they are not needed here
  return new Function("abap", "return " + js!.substring(from, to))({});
}

describe("Method metadata", () => {

  it("instance method without extras is unchanged", async () => {
    const abap = `
CLASS lcl DEFINITION.
  PUBLIC SECTION.
    METHODS run IMPORTING bar TYPE string.
ENDCLASS.
CLASS lcl IMPLEMENTATION.
  METHOD run.
  ENDMETHOD.
ENDCLASS.`;
    const js = await runSingle(abap);
    expect(js).to.include(`static METHODS = {"RUN": {"visibility": "U", "parameters": {"BAR": {"type": () => {return new abap.types.String({qualifiedName: "STRING"});}, "is_optional": " ", "parm_kind": "I", "type_name": "StringType"}}}};`);
  });

  it("optional parameters, OPTIONAL and DEFAULT", async () => {
    const abap = `
CLASS lcl DEFINITION.
  PUBLIC SECTION.
    METHODS run
      IMPORTING
        required TYPE i
        extra    TYPE i OPTIONAL
        preset   TYPE i DEFAULT 2.
ENDCLASS.
CLASS lcl IMPLEMENTATION.
  METHOD run.
  ENDMETHOD.
ENDCLASS.`;
    const parameters = (await methodsOf(abap, "lcl")).RUN.parameters;
    expect(parameters.REQUIRED.is_optional).to.equal(" ");
    expect(parameters.EXTRA.is_optional).to.equal("X");
    expect(parameters.PRESET.is_optional).to.equal("X");
  });

  it("optional parameter written in upper case", async () => {
    const abap = `
CLASS lcl DEFINITION.
  PUBLIC SECTION.
    METHODS run IMPORTING EXTRA TYPE i OPTIONAL.
ENDCLASS.
CLASS lcl IMPLEMENTATION.
  METHOD run.
  ENDMETHOD.
ENDCLASS.`;
    expect((await methodsOf(abap, "lcl")).RUN.parameters.EXTRA.is_optional).to.equal("X");
  });

  it("static method of a class", async () => {
    const abap = `
CLASS lcl DEFINITION.
  PUBLIC SECTION.
    CLASS-METHODS version RETURNING VALUE(result) TYPE string.
    METHODS run.
ENDCLASS.
CLASS lcl IMPLEMENTATION.
  METHOD version.
  ENDMETHOD.
  METHOD run.
  ENDMETHOD.
ENDCLASS.`;
    const methods = await methodsOf(abap, "lcl");
    expect(methods.VERSION.is_class).to.equal("X");
    expect(methods.RUN.is_class).to.equal(undefined);
  });

  it("static method of an interface", async () => {
    const abap = `
INTERFACE lif.
  CLASS-METHODS version RETURNING VALUE(result) TYPE string.
  METHODS run.
ENDINTERFACE.`;
    const methods = await methodsOf(abap, "lif");
    expect(methods.VERSION.is_class).to.equal("X");
    expect(methods.RUN.is_class).to.equal(undefined);
  });

  it("RAISING clause", async () => {
    const abap = `
INTERFACE lif.
  METHODS run
    RAISING
      cx_sy_zerodivide
      cx_sy_conversion_error.
  METHODS quiet.
ENDINTERFACE.`;
    const methods = await methodsOf(abap, "lif");
    expect(methods.RUN.exceptions).to.deep.equal(["CX_SY_ZERODIVIDE", "CX_SY_CONVERSION_ERROR"]);
    expect(methods.QUIET.exceptions).to.equal(undefined);
  });

  it("alias for an interface method, in an interface", async () => {
    const abap = `
INTERFACE lif_base.
  METHODS run
    IMPORTING extra TYPE i OPTIONAL
    RAISING cx_sy_zerodivide.
ENDINTERFACE.
INTERFACE lif.
  INTERFACES lif_base.
  ALIASES go FOR lif_base~run.
ENDINTERFACE.`;
    const methods = await methodsOf(abap, "lif");
    expect(methods.GO.alias_for).to.equal("LIF_BASE~RUN");
    expect(methods.GO.visibility).to.equal("U");
    expect(methods.GO.exceptions).to.deep.equal(["CX_SY_ZERODIVIDE"]);
    expect(methods.GO.parameters.EXTRA.is_optional).to.equal("X");
  });

  it("alias for an interface method, in a class", async () => {
    const abap = `
INTERFACE lif_base.
  METHODS run.
ENDINTERFACE.
CLASS lcl DEFINITION.
  PUBLIC SECTION.
    INTERFACES lif_base.
    ALIASES go FOR lif_base~run.
    METHODS own.
ENDCLASS.
CLASS lcl IMPLEMENTATION.
  METHOD lif_base~run.
  ENDMETHOD.
  METHOD own.
  ENDMETHOD.
ENDCLASS.`;
    const methods = await methodsOf(abap, "lcl");
    expect(methods.GO.alias_for).to.equal("LIF_BASE~RUN");
    expect(methods.OWN.alias_for).to.equal(undefined);
  });

  it("alias for an attribute is not a method", async () => {
    const abap = `
INTERFACE lif_base.
  DATA counter TYPE i.
  METHODS run.
ENDINTERFACE.
INTERFACE lif.
  INTERFACES lif_base.
  ALIASES count FOR lif_base~counter.
ENDINTERFACE.`;
    const methods = await methodsOf(abap, "lif");
    expect(Object.keys(methods)).to.deep.equal([]);
  });

});
