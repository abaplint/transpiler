import {Character} from "./types";

// TEST Map()

export class CharacterFactory {
  private static readonly map = new Map<string, Character>();

  public static get(num: number, value: string): Character {
    const key = num + value;
    const lookup = CharacterFactory.map.get(key);
    if (lookup === undefined) {
      const char = new Character(num).set(value).setConstant();
      CharacterFactory.map.set(key, char);
      return char;
    } else {
      return lookup;
    }
  }
/*
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
*/
}