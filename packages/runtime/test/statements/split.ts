import {expect} from "chai";
import {ABAP} from "../../src";

describe("Statement SPLIT", () => {

  it("split", () => {
    const abap = new ABAP();
    const tab = new abap.types.Table(new abap.types.String());
    abap.statements.split({source: `foo bar`, at: ` `, target: tab});
    const arr = tab.array();
    expect(arr.length).to.equal(2);
    expect(arr[0].get()).to.equal("foo");
  });

  it("split, String", () => {
    const abap = new ABAP();
    const tab = new abap.types.Table(new abap.types.String());
    const str = new abap.types.String();
    str.set(`foo bar`);
    abap.statements.split({source: str, at: ` `, target: tab});

    const arr = tab.array();
    expect(arr.length).to.equal(2);
    expect(arr[0].get()).to.equal("foo");
  });

});

