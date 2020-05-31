import {expect} from "chai";
import {Transpiler} from "../src";

describe("Require", () => {

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

    const output = await new Transpiler().run(files);
    expect(output.length).to.equal(2);
    expect(output[0].js.contents).to.contain("export class zcl_foo ");
    expect(output[1].js.contents).to.contain("export class zcl_bar ");

    expect(output[0].requires.length).to.equal(1, "expected one require");
    expect(output[0].requires[0].type).to.equal("CLAS");
    expect(output[0].requires[0].name).to.equal("ZCL_BAR");
  });

});
