import {ICharacter} from "../types/_character";

export interface IMessageOptions {
  id?: ICharacter | string,
  number?: ICharacter | string,
  type?: ICharacter | string,
  with?: (ICharacter | string)[],
  into?: ICharacter,
}

export function message(options: IMessageOptions): void {
  // todo
  if (options.into) {
    options.into.set("TodoRuntimeMessageINTO");
  }
}