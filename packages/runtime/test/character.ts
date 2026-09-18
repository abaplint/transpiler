import {expect} from "chai";
import {ABAP} from "../src";

describe("Character", () => {
  const abap = new ABAP();
  const float = (v: number): any => new abap.types.Float().set(v);
  const add = (l: any, r: any): any => abap.operators.add(l, r);
  const multiply = (l: any, r: any): any => abap.operators.multiply(l, r);

  it("a constant literal is read as a number, twice", () => {
    const half: any = abap.CharacterFactory.get(3, "0.5");
    expect(multiply(half, float(4)).getRaw()).to.equal(2);
    // again, this time from what the constant remembered
    expect(multiply(half, float(4)).getRaw()).to.equal(2);
    expect(add(half, float(0.25)).getRaw()).to.equal(0.75);
    expect(half.getNumeric()).to.equal(0.5);
  });

  it("a constant cannot change", () => {
    const half: any = abap.CharacterFactory.get(3, "0.5");
    expect(() => half.set("0.2")).to.throw();
    expect(half.getNumeric()).to.equal(0.5);
  });

  it("a character written after being read gives the new value", () => {
    const c: any = new abap.types.Character(5);
    c.set("0.5");
    expect(multiply(c, float(4)).getRaw()).to.equal(2);
    c.set("0.25");
    expect(multiply(c, float(4)).getRaw()).to.equal(1);
    c.clear();
    expect(multiply(c, float(4)).getRaw()).to.equal(0);
    c.set("2");
    expect(multiply(c, float(4)).getRaw()).to.equal(8);
  });

  it("a character written at an offset after being read gives the new value", () => {
    const c: any = new abap.types.Character(5);
    c.set("0.5");
    expect(c.getNumeric()).to.equal(0.5);
    new abap.OffsetLength(c, {offset: 2, length: 1}).set("2");
    expect(c.get()).to.equal("0.2  ");
    expect(c.getNumeric()).to.equal(0.2);
  });

  // clear() is the one write that is not stopped by the constant check, so it
  // is the one that has to drop what the constant remembered
  it("a constant that is cleared reads as zero afterwards", () => {
    const half: any = new abap.types.Character(3).set("0.5").setConstant();
    expect(half.getNumeric()).to.equal(0.5);
    half.clear();
    expect(half.get()).to.equal("   ");
    expect(half.getNumeric()).to.equal(0);
    expect(multiply(half, float(4)).getRaw()).to.equal(0);
  });

  it("a clone of a constant is a fresh, writable character", () => {
    const half: any = abap.CharacterFactory.get(3, "0.5");
    const c: any = half.clone();
    expect(c.getNumeric()).to.equal(0.5);
    c.set("0.2");
    expect(c.getNumeric()).to.equal(0.2);
    expect(half.getNumeric()).to.equal(0.5);
  });

  it("padding, blanks, integers and what is not a number", () => {
    const num = (length: number, value: string): number => {
      const c = new abap.types.Character(length);
      c.set(value);
      return c.getNumeric();
    };
    expect(num(5, "0.5")).to.equal(0.5);
    expect(num(1, " ")).to.equal(0);
    expect(num(3, "")).to.equal(0);
    expect(num(2, "12")).to.equal(12);
    expect(num(3, "-1")).to.equal(-1);
    expect(Number.isNaN(num(1, "X"))).to.equal(true);
  });
});
