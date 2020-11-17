import {expect} from "chai";
import * as abap from "../../src";

describe("Statement APPEND", () => {

  it("Append an integer", () => {
    const lt_letters = new abap.types.Table(new abap.types.Integer());
    abap.statements.append({source: 1, target: lt_letters});
    expect(lt_letters.array().length).to.equal(1);
  });

});

