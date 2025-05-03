import {Character} from "./types";

export class CharacterFactory {
  private static readonly map: {[num: string]: Character} = {};

  public static get(length: number, value: string): Character {
    if (CharacterFactory.map[value] === undefined) {
      CharacterFactory.map[value] = new Character(length).set(value).setConstant();
    }
    return CharacterFactory.map[value];
  }
}