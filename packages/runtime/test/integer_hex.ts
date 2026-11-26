import {expect} from "chai";
import {Hex, HexUInt8, Integer, Integer8, XString} from "../src/types";

// Expected bytes measured on ABAP 7.58, including unsigned short sources
// and zero padding (not sign extension) beyond the integer's width.
describe("Integer byte conversions", () => {
  const int8Cases: [bigint, string[]][] = [
    [-1n, ["FFFF", "FFFFFFFF", "FFFFFFFFFFFFFFFF", "0000000000000000FFFFFFFFFFFFFFFF"]],
    [-2n, ["FFFE", "FFFFFFFE", "FFFFFFFFFFFFFFFE", "0000000000000000FFFFFFFFFFFFFFFE"]],
    [0n, ["0000", "00000000", "0000000000000000", "00000000000000000000000000000000"]],
    [-9223372036854775808n, ["0000", "00000000", "8000000000000000", "00000000000000008000000000000000"]],
    [9223372036854775807n, ["FFFF", "FFFFFFFF", "7FFFFFFFFFFFFFFF", "00000000000000007FFFFFFFFFFFFFFF"]],
    [72623859790382856n, ["0708", "05060708", "0102030405060708", "00000000000000000102030405060708"]],
  ];
  const lengths = [2, 4, 8, 16];
  for (const Type of [Hex, HexUInt8]) {
    for (const [value, expected] of int8Cases) {
      for (const [index, length] of lengths.entries()) {
        it(`int8 ${value} to ${Type.name}(${length})`, () => {
          expect(new Type({length}).set(new Integer8().set(value)).get()).to.equal(expected[index]);
        });
      }
    }
    for (const [hex, expected] of [
      ["FFFE", 65534n], ["FFFFFFFF", 4294967295n],
      ["FFFFFFFFFFFFFFFF", -1n], ["FFFFFFFFFFFFFFFE", -2n],
      ["0000000000000000", 0n], ["8000000000000000", -9223372036854775808n],
      ["7FFFFFFFFFFFFFFF", 9223372036854775807n],
      ["0000000000000000FFFFFFFFFFFFFFFE", -2n],
      ["FF000000000000000000000000000001", 1n],
      ["FFFFFFFFFFFFFFFF8000000000000000", -9223372036854775808n],
      ["FFFFFFFFFFFFFFFF7FFFFFFFFFFFFFFF", 9223372036854775807n],
    ] as [string, bigint][]) {
      it(`${Type.name} ${hex} to int8`, () => {
        expect(new Integer8().set(new Type({length: hex.length / 2}).set(hex)).get()).to.equal(expected);
      });
    }
    for (const [value, expected] of [
      [-1, ["FFFF", "FFFFFFFF", "00000000FFFFFFFF", "000000000000000000000000FFFFFFFF"]],
      [-2, ["FFFE", "FFFFFFFE", "00000000FFFFFFFE", "000000000000000000000000FFFFFFFE"]],
      [0, ["0000", "00000000", "0000000000000000", "00000000000000000000000000000000"]],
      [-2147483648, ["0000", "80000000", "0000000080000000", "00000000000000000000000080000000"]],
      [2147483647, ["FFFF", "7FFFFFFF", "000000007FFFFFFF", "0000000000000000000000007FFFFFFF"]],
    ] as [number, string[]][]) {
      for (const [index, length] of lengths.entries()) {
        it(`i ${value} to ${Type.name}(${length})`, () => {
          expect(new Type({length}).set(new Integer().set(value)).get()).to.equal(expected[index]);
        });
      }
    }
    for (const [hex, expected] of [["FFFE", 65534], ["FFFFFFFF", -1], ["FFFFFFFE", -2],
      ["00000000", 0], ["80000000", -2147483648], ["7FFFFFFF", 2147483647],
      ["FFFFFFFFFFFFFFFFFFFFFFFF80000000", -2147483648]] as [string, number][]) {
      it(`${Type.name} ${hex} to i`, () => {
        expect(new Integer().set(new Type({length: hex.length / 2}).set(hex)).get()).to.equal(expected);
      });
    }
  }
  for (const [value, expected] of [[0n, "00"], [128n, "80"], [255n, "FF"],
    [2147483648n, "80000000"], [4294967295n, "FFFFFFFF"], [4294967296n, "0100000000"],
    [1099511627775n, "FFFFFFFFFF"], [1099511627776n, "010000000000"],
    [-1n, "FFFFFFFFFFFFFFFF"], [-2n, "FFFFFFFFFFFFFFFE"],
    [-9223372036854775808n, "8000000000000000"], [9223372036854775807n, "7FFFFFFFFFFFFFFF"]] as [bigint, string][]) {
    it(`int8 ${value} to xstring`, () => {
      expect(new XString().set(new Integer8().set(value)).get()).to.equal(expected);
    });
  }
  for (const [value, expected] of [[0, "00"], [5, "05"], [128, "80"], [255, "FF"],
    [256, "0100"], [32768, "8000"], [8388608, "800000"], [-1, "FFFFFFFF"],
    [-2, "FFFFFFFE"], [-128, "FFFFFF80"], [-129, "FFFFFF7F"],
    [-2147483648, "80000000"], [2147483647, "7FFFFFFF"]] as [number, string][]) {
    it(`i ${value} to xstring`, () => {
      expect(new XString().set(new Integer().set(value)).get()).to.equal(expected);
    });
  }
  for (const [hex, expected] of [["", 0n], ["FF", 255n], ["FFFFFFFFFFFFFFFF", -1n],
    ["0000000000000000FFFFFFFFFFFFFFFE", -2n], ["FF000000000000000000000000000001", 1n]] as [string, bigint][]) {
    it(`xstring ${hex} to int8`, () => {
      expect(new Integer8().set(new XString().set(hex)).get()).to.equal(expected);
    });
  }
});
