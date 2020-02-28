import {expect} from "chai";
import * as abap from "../../src/runtime";

describe("Arithmetics", () => {
  it("Set initial value", () => {
    const foo = new abap.types.Integer();
    const bar = new abap.types.Integer();
    foo.set(bar);

    expect(foo).to.not.equal(undefined);
    expect(foo.get()).to.equal(0);
  });

  it("2 + 2", () => {
    const foo = new abap.types.Integer({value: 2});
    const bar = new abap.types.Integer({value: 2});
    const boo = new abap.types.Integer({value: 2});

    boo.set(foo.add(bar));

    expect(boo.get()).to.equal(4);
  });
});