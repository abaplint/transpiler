import {Integer} from "./types";

export class IntegerFactory {
  private static readonly map: {[num: number]: Integer} = {};

  public static get(value: number): Integer {
    if (IntegerFactory.map[value] === undefined) {
      IntegerFactory.map[value] = new Integer().set(value).setConstant();
    }
    return IntegerFactory.map[value];
  }
}