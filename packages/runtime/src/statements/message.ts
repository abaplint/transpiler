import {ICharacter} from "../types/_character";

export interface IMessageOptions {
  into?: ICharacter,
}

export function message(options: IMessageOptions): void {
  // todo
  if (options.into) {
    options.into.set("TodoRuntimeMessageINTO");
  }
}