import {expect} from "chai";
import {ITranspilerOptions, UnknownTypesEnum} from "../src/types";
import {runSingle} from "./_utils";

const options: ITranspilerOptions = {unknownTypes: UnknownTypesEnum.runtimeError};

describe("Unknown types, errors at runtime", () => {

  it("test 1, ok", async () => {
    const abap = `DATA foo TYPE i.`;

    const expected = `let foo = new abap.types.Integer({qualifiedName: "I"});`;

    expect(await runSingle(abap, options)).to.equal(expected);
  });

  it("test 2, error", async () => {
    const abap = `DATA foo TYPE REF TO zcl_bar.`;

    const expected = `let foo = (() => { throw new Error("Void type: ZCL_BAR") })();`;

    expect(await runSingle(abap, options)).to.equal(expected);
  });

  it("test 3, CREATE OBJECT", async () => {
    const abap = `DATA foo TYPE REF TO object.
    CREATE OBJECT foo TYPE zcl_abapgit_gui.`;

    const expected = `let foo = new abap.types.ABAPObject({qualifiedName: undefined, RTTIName: undefined});
foo.set(await (new abap.Classes['ZCL_ABAPGIT_GUI']()).constructor_());`;

    expect(await runSingle(abap, options)).to.equal(expected);
  });

  it("test 4, CREATE OBJECT something", async () => {
    const abap = `DATA foo TYPE REF TO something.
    CREATE OBJECT foo.`;

    const expected = `let foo = (() => { throw new Error("Void type: SOMETHING") })();
foo.set(await (new abap.Classes['RUNTIME_ERROR']()).constructor_());`;

    expect(await runSingle(abap, options)).to.equal(expected);
  });

});