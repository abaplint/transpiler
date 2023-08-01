import {INumeric} from "../types/_numeric";

export async function wait(options: {cond: () => any, seconds?: INumeric}): Promise<void> {
  await new Promise(r => setTimeout(r, 50));
  while (true) {
    if (options.cond() === true) {
      break;
    }
    await new Promise(r => setTimeout(r, 500));
    console.log("WAIT waiting another round");
  }
}