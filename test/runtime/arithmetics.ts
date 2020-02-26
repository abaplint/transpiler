import {expect} from "chai";
import * as abap from "../../src/runtime";

describe("Arithmetics", () => {
  it("Set initial value", () => {
    let foo = new abap.basictypes.i();
    let bar = new abap.basictypes.i();
    foo.set(bar);

    expect(foo).to.not.equal(undefined);
    expect(foo.get()).to.equal(0);
  });

  it("2 + 2", () => {
    let foo = new abap.basictypes.i(2);
    let bar = new abap.basictypes.i(2);
    let boo = new abap.basictypes.i(2);

    boo.set(foo.add(bar));

    expect(boo.get()).to.equal(4);
  });
});