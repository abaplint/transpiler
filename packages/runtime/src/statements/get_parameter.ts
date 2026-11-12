import {ICharacter} from "../types/_character";
import {ABAP} from "..";

declare const abap: ABAP;

function parameterId(source: ICharacter): string {
  return source.get().toString().trimEnd();
}

/** GET PARAMETER ID pid FIELD dobj: the value from the user memory; none there: sy-subrc 4 and dobj initial */
export function getParameter(source: ICharacter, target: ICharacter) {
  const value = abap.context.userParameters.get(parameterId(source));
  if (value === undefined) {
    target.clear();
    abap.builtin.sy.get().subrc.set(4);
    return;
  }
  target.set(value);
  abap.builtin.sy.get().subrc.set(0);
}

/** SET PARAMETER ID pid FIELD dobj: puts the value into the user memory */
export function setParameter(source: ICharacter, value: ICharacter) {
  abap.context.userParameters.set(parameterId(source), value.get().toString());
}
