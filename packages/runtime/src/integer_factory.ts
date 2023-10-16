import {Integer} from "./types";

export class IntegerFactory {
  private static readonly map: {[num: number]: Integer} = {};

  public static get(num: number): Integer {
    if (IntegerFactory.map[num] === undefined) {
      IntegerFactory.map[num] = new Integer().set(num).setConstant();
    }
    return IntegerFactory.map[num];
  }
}