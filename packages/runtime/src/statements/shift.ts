import {ICharacter} from "../types/_character";

export interface IShiftOptions {
  deletingLeading?: string | ICharacter,
  direction?: "LEFT" | "RIGHT",
}

export function shift(target: ICharacter, options?: IShiftOptions) {
  if (options?.direction === "RIGHT") {
    throw "SHIFT, RIGHT todo";
  }
  if (options?.deletingLeading === undefined) {
    return;
  }

  let leading = options.deletingLeading;
  if (typeof leading !== "string") {
    leading = leading.get();
  }
  const split = leading.split("");

  let value = target.get();
  while(split.some(s => value.substr(0, 1) === s)) {
    value = value.substr(1);
  }
  target.set(value);

}