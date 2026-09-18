import {expect} from "chai";
import {ABAP} from "../src";

describe("Compare", () => {
  it("2 = 2", () => {
    const abap = new ABAP();
    const foo = new abap.types.Integer();
    foo.set(2);
    const bar = new abap.types.Integer();
    bar.set(2);
    const bool = abap.compare.eq(foo, bar);
    expect(bool).to.equal(true);
  });

  it("empty string", () => {
    const abap = new ABAP();
    const foo = new abap.types.String();
    const bool = abap.compare.eq(foo, "");
    expect(bool).to.equal(true);
  });

  // A character literal compared with a float is converted to a float, the
  // way ABAP converts the character operand to the numeric one's type.
  // It used to go through parseInt, so '0.5' was 0 and 0.06 > '0.5' held.
  it("float against a character literal with a fraction", () => {
    const abap = new ABAP();
    const float = (v: number): any => { const f = new abap.types.Float(); f.set(v); return f; };
    const char = (v: string): any => { const c = new abap.types.Character(v.length); c.set(v); return c; };
    expect(abap.compare.gt(float(0.06), char("0.5"))).to.equal(false);
    expect(abap.compare.lt(float(0.06), char("0.5"))).to.equal(true);
    expect(abap.compare.gt(float(0.6), char("0.5"))).to.equal(true);
    expect(abap.compare.gt(char("0.5"), float(0.06))).to.equal(true);
    expect(abap.compare.ge(float(0.5), char("0.5"))).to.equal(true);
    expect(abap.compare.le(float(0.5), char("0.5"))).to.equal(true);
    // an integer literal, the common case, is what it always was
    expect(abap.compare.gt(float(1.5), char("1"))).to.equal(true);
    expect(abap.compare.gt(float(0.5), char("1"))).to.equal(false);
    expect(abap.compare.gt(float(1), char(""))).to.equal(true);
  });
});
