import {ICharacter} from "../types/_character";

export function condense(input: ICharacter): void {
  input.set(input.get().trim());
}