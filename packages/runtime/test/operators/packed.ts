import {expect} from "chai";
import {ABAP} from "../../src";

// Arithmetic with a packed operand has calculation type p and is exact. It
// used to fall through to the float branch, and a double holds 15 to 17
// significant digits where a timestampl has 21: 20261014103400.1234567 +
// '0.0000001' answered .1235968, p MOD 100 answered 0, p * 1 changed p
describe("Operators - packed calculation type", () => {
  const abap = new ABAP();
  // throwError looks the exception class up on the global abap
  (globalThis as any).abap = abap;

  function timestampl(value: string) {
    return new abap.types.Packed({length: 11, decimals: 7}).set(value);
  }

  it("add a literal to a timestampl", () => {
    const p = timestampl("20261014103400.1234567");
    const r = abap.operators.add(p, "0.0000001");
    expect(r).to.be.instanceof(abap.types.Packed);
    expect((r as any).toFixed(7)).to.equal("20261014103400.1234568");
  });

  it("subtract a literal from a timestampl", () => {
    const p = timestampl("20261014103400.1234567");
    expect((abap.operators.minus(p, "0.0000001") as any).toFixed(7)).to.equal("20261014103400.1234566");
  });

  it("unary minus", () => {
    const p = timestampl("20261014103400.1234567");
    expect((abap.operators.minus(0, p) as any).toFixed(7)).to.equal("-20261014103400.1234567");
  });

  it("multiply by one keeps the value", () => {
    const p = timestampl("20261014103400.1234567");
    expect((abap.operators.multiply(p, 1) as any).toFixed(7)).to.equal("20261014103400.1234567");
    const one = new abap.types.Integer().set(1);
    expect((abap.operators.multiply(one, p) as any).toFixed(7)).to.equal("20261014103400.1234567");
  });

  it("MOD and DIV of a timestampl", () => {
    const p = timestampl("20261003101500.3500000");
    const hundred = new abap.types.Integer().set(100);
    expect((abap.operators.mod(p, hundred) as any).toFixed(7)).to.equal("0.3500000");
    expect((abap.operators.div(p, hundred) as any).toFixed(7)).to.equal("202610031015.0000000");
    expect((abap.operators.mod(p, 100) as any).toFixed(7)).to.equal("0.3500000");
    expect((abap.operators.div(p, 100) as any).toFixed(7)).to.equal("202610031015.0000000");
  });

  it("MOD and DIV with a negative divisor, the remainder is never negative", () => {
    const p = new abap.types.Packed({length: 8, decimals: 1}).set("7.5");
    expect((abap.operators.mod(p, -2) as any).toFixed(1)).to.equal("1.5");
    expect((abap.operators.div(p, -2) as any).toFixed(0)).to.equal("-3");
    const n = new abap.types.Packed({length: 8, decimals: 1}).set("-7.5");
    expect((abap.operators.mod(n, 2) as any).toFixed(1)).to.equal("0.5");
    expect((abap.operators.div(n, 2) as any).toFixed(0)).to.equal("-4");
  });

  it("two packed with decimals", () => {
    const a = new abap.types.Packed({length: 8, decimals: 2}).set("1.10");
    const b = new abap.types.Packed({length: 8, decimals: 2}).set("2.20");
    expect((abap.operators.add(a, b) as any).toFixed(2)).to.equal("3.30");
    expect((abap.operators.multiply(a, b) as any).toFixed(4)).to.equal("2.4200");
  });

  it("division, an integer by a packed", () => {
    const four = new abap.types.Packed({length: 8, decimals: 2}).set("4.00");
    const r = abap.operators.divide(10, four);
    expect(r).to.be.instanceof(abap.types.Packed);
    expect(r.get()).to.equal(2.5);
    const target = new abap.types.Packed({length: 8, decimals: 2}).set(r as any);
    expect(target.toFixed(2)).to.equal("2.50");
  });

  it("division of a timestampl is exact in the target", () => {
    const p = timestampl("20261003101500.3500000");
    const q = new abap.types.Packed({length: 11, decimals: 7}).set(abap.operators.divide(p, 100) as any);
    expect(q.toFixed(7)).to.equal("202610031015.0035000");
  });

  it("a float operand keeps the float calculation type", () => {
    const p = new abap.types.Packed({length: 8, decimals: 2}).set("1.50");
    const f = new abap.types.Float().set(2);
    const r = abap.operators.add(p, f);
    expect(r).to.be.instanceof(abap.types.Float);
    expect((r as any).getRaw()).to.equal(3.5);
  });

  it("through a field symbol", () => {
    const p = timestampl("20261014103400.1234567");
    const fs = new abap.types.FieldSymbol();
    fs.assign(p);
    expect((abap.operators.add(fs, 1) as any).toFixed(7)).to.equal("20261014103401.1234567");
    expect((abap.operators.multiply(fs as any, 1) as any).toFixed(7)).to.equal("20261014103400.1234567");
  });

  it("division by zero", () => {
    const p = new abap.types.Packed({length: 8, decimals: 2}).set("1.50");
    expect(() => abap.operators.divide(p, 0)).to.throw("CX_SY_ZERODIVIDE");
    expect(() => abap.operators.mod(p, 0)).to.throw("CX_SY_ZERODIVIDE");
    expect(() => abap.operators.div(p, 0)).to.throw("CX_SY_ZERODIVIDE");
    const zero = new abap.types.Packed({length: 8, decimals: 2});
    expect(abap.operators.divide(zero, 0).get()).to.equal(0);
  });
});
