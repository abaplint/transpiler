import {Character} from "./types";

export class CharacterFactory {
  private static readonly map: {[num: string]: Character} = {};

  public static get(value: string): Character {
    if (CharacterFactory.map[value] === undefined) {
      CharacterFactory.map[value] = new Character().set(value).setConstant();
    }
    return CharacterFactory.map[value];
  }
}