import {DecFloat34} from "./decfloat34";
import {Decimal, roundSignificant} from "./_decimal";

/** decfloat16 has 16 significant digits */
export const DECFLOAT16_PRECISION = 16;

/** A calculation with a decfloat16 operand is expected to run in decfloat34, the result is
 * rounded to 16 significant digits, half away from zero, when it is stored in the field */
export class DecFloat16 extends DecFloat34 {

  public clone(): DecFloat16 {
    const n = new DecFloat16();
    n.setDecimal(this.getDecimal());
    return n;
  }

  public set(value: Parameters<DecFloat34["set"]>[0]) {
    super.set(value);
    this.setDecimal(this.getDecimal());
    return this;
  }

  public setDecimal(value: Decimal): DecFloat16 {
    super.setDecimal(roundSignificant(value, DECFLOAT16_PRECISION));
    return this;
  }
}
