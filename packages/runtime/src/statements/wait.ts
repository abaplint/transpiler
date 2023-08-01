import {INumeric} from "../types/_numeric";

export async function wait(_options: {cond: () => any, seconds?: INumeric}): Promise<void> {
  await new Promise(r => setTimeout(r, 100));

  throw new Error("runtime wait(), todo");
}