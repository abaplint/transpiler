import {expect} from "chai";
import {run, IFixture} from "./compiler";

describe("ABAP", () => {

  it("Simple PROG", async () => {
    const fix: IFixture = {
      entryFilename: "zprogram.prog.abap",
      files: [{
        filename: "zprogram.prog.abap",
        contents: "WRITE 'sdf'."}],
    };
    const stats = await run(fix);

    const output = stats.toJson().modules![0].source;
    expect(output).to.equal(`abap.statements.write('sdf');`);
  });

  it("Parser error", async () => {
    const fix: IFixture = {
      entryFilename: "zprogram.prog.abap",
      files: [{
        filename: "zprogram.prog.abap",
        contents: "parser error!!!"}],
    };
    await run(fix)
      .then(() => expect.fail("expected error"))
      .catch((reason) => expect(reason.toString()).to.contain("arser"));
  });

  it("Simple CLAS", async () => {
    const fix: IFixture = {
      entryFilename: "zcl_foobar.clas.abap",
      files: [{
        filename: "zcl_foobar.clas.abap",
        contents: `
CLASS zcl_foobar DEFINITION PUBLIC FINAL CREATE PUBLIC.
ENDCLASS.
CLASS zcl_foobar IMPLEMENTATION.
ENDCLASS.`}],
    };
    const stats = await run(fix);

    const output = stats.toJson().modules![0].source;
    expect(output).to.contain(`export class`);
  });

  it("CLAS + INTF", async () => {
    const fix: IFixture = {
      entryFilename: "zcl_foobar.clas.abap",
      files: [{
        filename: "zcl_foobar.clas.abap",
        contents: `
CLASS zcl_foobar DEFINITION PUBLIC FINAL CREATE PUBLIC.
  PUBLIC SECTION.
    INTERFACES zif_foobar.
ENDCLASS.
CLASS zcl_foobar IMPLEMENTATION.
  METHOD zif_foobar~run.
  ENDMETHOD.
ENDCLASS.`}, {
        filename: "zif_foobar.intf.abap",
        contents: `
INTERFACE zif_foobar PUBLIC..
  METHODS: run.
ENDINTERFACE.`}],
    };
    const stats = await run(fix);

    const output = stats.toJson().modules![0].source;
    expect(output).to.contain(`export class`);
  });


});