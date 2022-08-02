import {expect} from "chai";
import {ITranspilerOptions} from "../src/types";
import {runSingle} from "./_utils";

const options: ITranspilerOptions = {unknownTypes: "runtimeError"};

describe("Unknown types, errors at runtime", () => {

  it("test 1, ok", async () => {
    const abap = `DATA foo TYPE i.`;

    const expected = `let foo = new abap.types.Integer({qualifiedName: "I"});`;

    expect(await runSingle(abap, options)).to.equal(expected);
  });

  it("test 2, error", async () => {
    const abap = `DATA foo TYPE REF TO zcl_bar.`;

    const expected = `let foo = (() => { throw "Void type: zcl_bar" })();`;

    expect(await runSingle(abap, options)).to.equal(expected);
  });

  it("test 3, CREATE OBJECT", async () => {
    const abap = `DATA foo TYPE REF TO object.
    CREATE OBJECT foo TYPE zcl_abapgit_gui.`;

    const expected = `let foo = new abap.types.ABAPObject();
foo.set(await (new abap.Classes['ZCL_ABAPGIT_GUI']()).constructor_());`;

    expect(await runSingle(abap, options)).to.equal(expected);
  });

  it("test 4, CREATE OBJECT something", async () => {
    const abap = `DATA foo TYPE REF TO something.
    CREATE OBJECT foo.`;

    const expected = `let foo = (() => { throw "Void type: something" })();
foo.set(await (new abap.Classes['RUNTIME_ERROR']()).constructor_());`;

    expect(await runSingle(abap, options)).to.equal(expected);
  });

});