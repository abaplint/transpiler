import {ICharacter} from "../types/_character.js";

export function getParameter(_source: ICharacter, _target: ICharacter) {
  // todo, additional logic? call ABAP kernel class?

  // @ts-ignore
  abap.builtin.sy.get().subrc.set(4);
}