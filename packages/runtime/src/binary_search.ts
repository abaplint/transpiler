import {ge, le} from "./compare";
import {ICharacter} from "./types/_character";
import {INumeric} from "./types/_numeric";

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