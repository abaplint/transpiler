import {ICharacter} from "../types/_character";
import {ABAP} from "..";

declare const abap: ABAP;

export function getParameter(_source: ICharacter, _target: ICharacter) {
  // todo, additional logic? call ABAP kernel class?

  abap.builtin.sy.get().subrc.set(4);
}