import {expect} from "chai";
import {ABAP} from "../../src";

// i is a JS number in the runtime, and the range check in set( ) was commented out:
// 2147483647 + 1 answered 2147483648 and a p with 11 digits fitted into an i, where a
// system raises CX_SY_ARITHMETIC_OVERFLOW and CX_SY_CONVERSION_OVERFLOW.
//
// The calculation type of + - * depends on the target as well, f = i * i is computed in
// f, so an operator returns its result unchecked and the check is made when the result
// is moved to an i
describe("Type i - overflow", () => {
  const abap = new ABAP();
  // throwError looks the exception class up on the global abap; the classes are not
  // registered here, so the lookup fails with the name of the class
  (globalThis as any).abap = abap;

  // the operators return a union the typed set( ) does not take
  const ops: any = abap.operators;

  const MAX = 2147483647;
  const MIN = -2147483648;

  function int(value: number) {
    return new abap.types.Integer().set(value);
  }

  it("the limits fit", () => {
    expect(int(MAX).get()).to.equal(MAX);
    expect(int(MIN).get()).to.equal(MIN);
    expect(int(0).set(ops.add(int(MAX), int(0))).get()).to.equal(MAX);
    expect(int(0).set(ops.minus(int(MIN), int(0))).get()).to.equal(MIN);
  });

  it("i = i + 1 past the maximum raises CX_SY_ARITHMETIC_OVERFLOW", () => {
    const i = int(42);
    expect(() => i.set(ops.add(int(MAX), int(1)))).to.throw("Global class CX_SY_ARITHMETIC_OVERFLOW not found");
    expect(i.get()).to.equal(42);
    expect(() => i.set(ops.add(int(MAX), 1))).to.throw("CX_SY_ARITHMETIC_OVERFLOW");
    expect(() => i.set(ops.add(1, int(MAX)))).to.throw("CX_SY_ARITHMETIC_OVERFLOW");
    expect(() => i.set(ops.add(MAX, 1))).to.throw("CX_SY_ARITHMETIC_OVERFLOW");
    const fs = new abap.types.FieldSymbol();
    fs.assign(i);
    expect(() => fs.set(ops.add(int(MAX), int(1)))).to.throw("CX_SY_ARITHMETIC_OVERFLOW");
  });

  it("i = i - 1 past the minimum raises CX_SY_ARITHMETIC_OVERFLOW", () => {
    expect(() => int(0).set(ops.minus(int(MIN), int(1)))).to.throw("CX_SY_ARITHMETIC_OVERFLOW");
    expect(() => int(0).set(ops.minus(int(MIN), 1))).to.throw("CX_SY_ARITHMETIC_OVERFLOW");
    expect(() => int(0).set(ops.minus(0, int(MIN)))).to.throw("CX_SY_ARITHMETIC_OVERFLOW");
  });

  it("i = i * i past the range raises CX_SY_ARITHMETIC_OVERFLOW", () => {
    expect(() => int(0).set(ops.multiply(int(65536), int(65536)))).to.throw("CX_SY_ARITHMETIC_OVERFLOW");
    expect(() => int(0).set(ops.multiply(int(65536), 65536))).to.throw("CX_SY_ARITHMETIC_OVERFLOW");
    expect(int(0).set(ops.multiply(int(46341), int(46340))).get()).to.equal(2147441940);
    expect(() => int(0).set(ops.div(int(MIN), int(-1)))).to.throw("CX_SY_ARITHMETIC_OVERFLOW");
  });

  it("f = i * i is computed in f, the target decides the calculation type", () => {
    const f = new abap.types.Float().set(ops.multiply(int(2000000), int(2000000)));
    expect(f.getRaw()).to.equal(4000000000000);
    const p = new abap.types.Packed({length: 8, decimals: 0}).set(ops.add(int(MAX), int(1)));
    expect(p.toFixed(0)).to.equal("2147483648");
    const s = new abap.types.String().set(ops.add(int(MAX), int(1)));
    expect(s.get()).to.equal("2147483648 ");
  });

  it("a conversion that does not fit raises CX_SY_CONVERSION_OVERFLOW and keeps the value", () => {
    const i = int(42);
    expect(() => i.set("10000000000")).to.throw("Global class CX_SY_CONVERSION_OVERFLOW not found");
    expect(i.get()).to.equal(42);
    expect(() => i.set(new abap.types.String().set("2147483648"))).to.throw("CX_SY_CONVERSION_OVERFLOW");
    expect(() => i.set(new abap.types.Packed({length: 8, decimals: 0}).set("10000000000"))).to.throw("CX_SY_CONVERSION_OVERFLOW");
    expect(() => i.set(new abap.types.Float().set(3000000000))).to.throw("CX_SY_CONVERSION_OVERFLOW");
    expect(() => i.set(new abap.types.Integer8().set(2147483648n) as any)).to.throw("CX_SY_CONVERSION_OVERFLOW");
    expect(() => i.set(-2147483649)).to.throw("CX_SY_CONVERSION_OVERFLOW");
    expect(i.get()).to.equal(42);
  });

  it("a float is rounded before the check", () => {
    expect(int(2147483647.4).get()).to.equal(MAX);
    expect(() => int(2147483647.5)).to.throw("CX_SY_CONVERSION_OVERFLOW");
  });

  it("int8 is not affected", () => {
    const i8 = new abap.types.Integer8().set(2147483647n);
    expect(ops.add(i8, 1).get()).to.equal(2147483648n);
    expect(ops.multiply(i8, i8).get()).to.equal(4611686014132420609n);
  });

  it("a hex is read as the last four bytes in two's complement", () => {
    expect(int(0).set(new abap.types.Hex({length: 4}).set("FFFFFFFF")).get()).to.equal(-1);
    expect(int(0).set(new abap.types.Hex({length: 4}).set("80000000")).get()).to.equal(MIN);
    expect(int(0).set(new abap.types.Hex({length: 4}).set("7FFFFFFF")).get()).to.equal(MAX);
  });

  it("DIV and MOD with a large packed operand into a p target", () => {
    const p = new abap.types.Packed({length: 11, decimals: 0}).set("10000000000");
    const q = new abap.types.Packed({length: 11, decimals: 0});
    expect(q.set(ops.div(p, 1)).toFixed(0)).to.equal("10000000000");
    expect(q.set(ops.mod(int(5), p)).toFixed(0)).to.equal("5");
  });
});
