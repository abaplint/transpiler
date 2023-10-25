import {Character} from "./types";

export class CharacterFactory {
  private static readonly map: {[key: string]: Character} = {};

  public static get(num: number, value: string): Character {
    const key = num + value;
    if (CharacterFactory.map[key] === undefined) {
      CharacterFactory.map[key] = new Character(num).set(value).setConstant();
    }
    return CharacterFactory.map[key];
  }
}