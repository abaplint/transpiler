import {INumeric} from "../types/_numeric";
import {ABAP} from "..";

declare const abap: ABAP;

export async function wait(options: {cond: () => any, seconds?: INumeric}): Promise<void> {
  const timeout = options.seconds === undefined ? undefined : options.seconds.get() * 1000;
  const deadline = timeout === undefined ? undefined : Date.now() + timeout;

  while (true) {
    if (options.cond() === true) {
      abap.builtin.sy.get().subrc.set(0);
      return;
    }

    const remaining = deadline === undefined ? 500 : deadline - Date.now();
    if (remaining <= 0) {
      abap.builtin.sy.get().subrc.set(8);
      return;
    }

    await new Promise(r => setTimeout(r, Math.min(500, remaining)));
  }
}
