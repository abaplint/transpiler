import {Hex, XString} from "../types/index.js";

export function hexToBinary(input: Hex | XString): string {
  let ret = "";
  const hex = input.get();
  for (let index = 0; index < hex.length / 2; index++) {
    const byte = hex.substring(index * 2, index * 2 + 2);
    ret += parseInt(byte, 16).toString(2).padStart(8, "0");
  }
  return ret;
}

// bitwise compare
export function m(operand1: Hex | XString, operand2: Hex | XString): boolean {
  let operand1Bits = hexToBinary(operand1);
  const operand2Bits = hexToBinary(operand2);

  if (operand1Bits.length < operand2Bits.length) {
    operand1Bits = operand1Bits.padEnd(operand2Bits.length, "0");
  }

  let oneFound = false;
  let zeroFound = false;
  for (let index = 0; index < operand2Bits.length; index++) {
    const o1bit = operand1Bits.substring(index, index + 1);
    const o2bit = operand2Bits.substring(index, index + 1);

    if (o2bit === "1") {
      if (o1bit === "1") {
        oneFound = true;
      } else {
        zeroFound = true;
      }
    }
  }

  return oneFound && zeroFound;
}