import {Hex, XString} from "../types";
import {hexToBinary} from "./m";

// bitwise compare
export function z(operand1: Hex | XString, operand2: Hex | XString): boolean {
  let operand1Bits = hexToBinary(operand1);
  const operand2Bits = hexToBinary(operand2);

  if (operand1Bits.length < operand2Bits.length) {
    operand1Bits = operand1Bits.padEnd(operand2Bits.length, "0");
  }

  let oneFound = false;
//  let zeroFound = false;
  for (let index = 0; index < operand2Bits.length; index++) {
    const o1bit = operand1Bits.substring(index, index + 1);
    const o2bit = operand2Bits.substring(index, index + 1);

    if (o2bit === "1") {
      if (o1bit === "1") {
        oneFound = true;
      } else {
//        zeroFound = true;
      }
    }
  }

  return oneFound === false;
}