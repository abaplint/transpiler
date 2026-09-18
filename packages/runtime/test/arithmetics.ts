import {expect} from "chai";
import {ABAP} from "../src";

describe("Arithmetics", () => {

  it("Set initial value", () => {
    const abap = new ABAP();
    const foo = new abap.types.Integer();
    const bar = new abap.types.Integer();
    foo.set(bar);

    expect(foo).to.not.equal(undefined);
    expect(foo.get()).to.equal(0);
  });


  // MOD follows the calculation type of its operands: with a float operand
  // the remainder is a float. It used to come back in an Integer, so
  // 2.75 MOD 1 was 1.
  it("MOD with a float operand answers a float", () => {
    const abap = new ABAP();
    const f = new abap.types.Float();
    f.set(2.75);
    const one = new abap.types.Integer();
    one.set(1);
    const r = abap.operators.mod(f as any, one);
    expect(r).to.be.instanceof(abap.types.Float);
    expect((r as any).getRaw()).to.equal(0.75);
    const i = new abap.types.Integer();
    i.set(7);
    const two = new abap.types.Integer();
    two.set(2);
    expect(abap.operators.mod(i, two).get()).to.equal(1);
  });

  it("MOD with an int8 operand answers int8", () => {
    const abap = new ABAP();
    const i8 = new abap.types.Integer8();
    i8.set(7n);
    const two = new abap.types.Integer();
    two.set(2);
    const r = abap.operators.mod(i8 as any, two);
    expect(r).to.be.instanceof(abap.types.Integer8);
    expect((r as any).get()).to.equal(1n);
  });

  // A float exactly on a half is rounded away from zero when it is moved to
  // an integer, on both sides: 0.5 is 1 and -0.5 is -1. Math.round rounds a
  // half towards positive infinity, so -0.5 used to be 0.
  it("a negative half moved to an integer rounds away from zero", () => {
    const abap = new ABAP();
    const move = (v: number) => {
      const f = new abap.types.Float();
      f.set(v);
      const i = new abap.types.Integer();
      i.set(f);
      return i.get();
    };
    expect(move(0.5)).to.equal(1);
    expect(move(1.5)).to.equal(2);
    expect(move(-0.5)).to.equal(-1);
    expect(move(-1.5)).to.equal(-2);
    expect(move(-0.4)).to.equal(0);
    expect(move(2.4)).to.equal(2);
  });
  // Two floats are answered by a branch of their own in add, minus and
  // multiply, immediately after the Integer/Integer case. It computes what the
  // rest of the chain would have computed, so the values, the result types and
  // the zero divisor must not move. divide is unchanged and is here because
  // the same cases have to keep holding for it.
  describe("float fast path", () => {
    const abap = new ABAP();
    const float = (v: number): any => new abap.types.Float().set(v);
    const int = (v: number): any => new abap.types.Integer().set(v);
    const char = (v: string): any => new abap.types.Character(v.length).set(v);
    const add = (l: any, r: any): any => abap.operators.add(l, r);
    const minus = (l: any, r: any): any => abap.operators.minus(l, r);
    const multiply = (l: any, r: any): any => abap.operators.multiply(l, r);
    const divide = (l: any, r: any): any => abap.operators.divide(l, r);

    it("two floats give a float", () => {
      expect(add(float(0.5), float(0.25))).to.be.instanceof(abap.types.Float);
      expect(add(float(0.5), float(0.25)).getRaw()).to.equal(0.75);
      expect(minus(float(0.5), float(0.25))).to.be.instanceof(abap.types.Float);
      expect(minus(float(0.5), float(0.25)).getRaw()).to.equal(0.25);
      expect(multiply(float(0.5), float(0.25))).to.be.instanceof(abap.types.Float);
      expect(multiply(float(0.5), float(0.25)).getRaw()).to.equal(0.125);
      expect(divide(float(0.5), float(0.25))).to.be.instanceof(abap.types.Float);
      expect(divide(float(0.5), float(0.25)).getRaw()).to.equal(2);
      expect(minus(float(0.25), float(0.5)).getRaw()).to.equal(-0.25);
      expect(add(float(0.1), float(0.2)).getRaw()).to.equal(0.1 + 0.2);
    });

    it("a float division is not an integer calculation", () => {
      // 1 / 4 of two floats is 0.25, it is not rounded the way i / i is
      const div = divide(float(1), float(4));
      expect(div.getRaw()).to.equal(0.25);
      expect(div.getCalculationValue()).to.equal(0.25);
    });

    it("integer operands keep the integer semantics", () => {
      expect(add(int(1), int(2))).to.be.instanceof(abap.types.Integer);
      expect(add(int(1), int(2)).get()).to.equal(3);
      expect(multiply(int(3), int(2))).to.be.instanceof(abap.types.Integer);
      // ABAP calculates in type i when all operands are integers, 3 / 2 is 2
      expect(divide(int(3), int(2)).getCalculationValue()).to.equal(2);
      // one integer and one float is a float, as before
      expect(add(int(1), float(0.5)).getRaw()).to.equal(1.5);
      expect(add(float(0.5), int(1)).getRaw()).to.equal(1.5);
      expect(divide(float(3), int(2)).getCalculationValue()).to.equal(1.5);
    });

    it("character operands keep the character semantics", () => {
      expect(add(char("2"), int(1)).get()).to.equal(3);
      expect(add(char("0.5"), float(0.5)).getRaw()).to.equal(1);
      expect(multiply(char("0.5"), float(4)).getRaw()).to.equal(2);
      expect(divide(char("1"), float(4)).getRaw()).to.equal(0.25);
      expect(minus(float(1), char("0.5")).getRaw()).to.equal(0.5);
    });

    it("a packed or a decfloat operand is not a float", () => {
      const packed: any = new abap.types.Packed({length: 8, decimals: 2}).set("0.5");
      expect(add(packed, float(0.5)).getRaw()).to.equal(1);
      expect(multiply(float(0.5), packed).getRaw()).to.equal(0.25);
      const dec: any = new abap.types.DecFloat34().set("0.5");
      expect(minus(dec, float(0.25)).getRaw()).to.equal(0.25);
      expect(divide(float(1), dec).getRaw()).to.equal(2);
    });

    it("dividing a float by zero", () => {
      expect(divide(float(0), float(0)).get()).to.equal(0);
      expect(() => divide(float(1), float(0))).to.throw();
      expect(() => divide(float(1), float(-0))).to.throw();
    });

    it("the exact value of an integer division survives further float arithmetic", () => {
      // divide() keeps the exact value and marks the calculation type, the
      // operators read the exact value, and so must the fast path
      const div = divide(int(3), int(2));
      expect(multiply(div, float(2)).getRaw()).to.equal(3);
      expect(add(div, float(0.5)).getRaw()).to.equal(2);
    });
  });
});
