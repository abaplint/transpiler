import {Character} from "./types";

export class CharacterFactory {
  private static readonly map: {[key: string]: Character} = {};

  public static get(num: number, value: string): Character {
    const key = num + value;
    const lookup = CharacterFactory.map[key];
    if (lookup === undefined) {
      CharacterFactory.map[key] = new Character(num).set(value).setConstant();
      return CharacterFactory.map[key];
    } else {
      return lookup;
    }
  }
}