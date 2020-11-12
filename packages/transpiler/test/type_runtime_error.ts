import {expect} from "chai";
import {ITranspilerOptions} from "../src";
import {runSingle} from "./_utils";

const options: ITranspilerOptions = {unknownTypes: "runtimeError"};

describe("Unknown types, errors at runtime", () => {

  it("test 1, ok", async () => {
    const abap = `DATA foo TYPE i.`;

    const expected = `let foo = new abap.types.Integer();`;

    expect(await runSingle(abap, options)).to.equal(expected);
  });

  it("test 2, error", async () => {
    const abap = `DATA foo TYPE REF TO zcl_bar.`;

    const expected = `let foo = (() => { throw "Unknown type: REF, unable to resolve zcl_bar" })();`;

    expect(await runSingle(abap, options)).to.equal(expected);
  });

});