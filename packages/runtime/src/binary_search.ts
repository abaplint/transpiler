/* eslint-disable max-len */
import {ge, le} from "./compare";
import {Structure} from "./types";
import {ICharacter} from "./types/_character";
import {INumeric} from "./types/_numeric";

export function binarySearchFromRow(array: readonly any[], left: number, right: number, keyField: (i: any) => any, keyValue: INumeric | ICharacter, usesTableLine: boolean | undefined) {
  const isStructured = array[0] instanceof Structure;
  while (right - left > 1) {
    const middle = Math.floor(((right - left) / 2) + left);
    const a = array[middle];
    let row: any = undefined;
    if (usesTableLine === false && isStructured === true) {
      row = a.get();
    } else {
      row = isStructured ? {table_line: a, ...a.get()} : {table_line: a};
    }
    if (ge(keyField(row), keyValue)) {
      right = middle;
    } else {
      left = middle;
    }
  }
  return right;
}

export function binarySearchFrom(array: readonly any[], left: number, right: number, keyField: string, keyValue: INumeric | ICharacter) {
  while (right - left > 1) {
    const middle = Math.floor(((right - left) / 2) + left);
    if (ge(array[middle].get()[keyField], keyValue)) {
      right = middle;
    } else {
      left = middle;
    }
  }
  return right;
}

export function binarySearchTo(array: readonly any[], left: number, right: number, keyField: string, keyValue: INumeric | ICharacter) {
  while (right - left > 1) {
    const middle = Math.floor(((right - left) / 2) + left);
    if (le(array[middle].get()[keyField], keyValue)) {
      left = middle;
    } else {
      right = middle;
    }
  }
  return right;
}