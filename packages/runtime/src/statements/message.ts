import {Context} from "../context";
import {ICharacter} from "../types/_character";

export interface IMessageOptions {
  id?: ICharacter | string,
  number?: ICharacter | string,
  type?: ICharacter | string,
  with?: (ICharacter | string)[],
  into?: ICharacter,
}

function replace(text: string, w?: (ICharacter | string)[]): string {
  for (let i = 0; i < 6; i++) {
    const search = "&" + (i + 1);
    let replace = "";
    if (w && w[i]) {
      const j = w[i];
      if (typeof j === "string") {
        replace = j;
      } else {
        replace = j.get();
      }
    }

    const field = "msgv" + (i + 1);
    if (i <= 3) {
      // @ts-ignore
      abap.builtin.sy.get()[field].set(replace);
    }

    text = text.replace(search, replace);
  }
  return text.trim();
}

function findText(context: Context, arbgb: string | undefined, msgnr: string | undefined): string {
  let text: string | undefined = undefined;
  const db = context.db;
  if (db && arbgb && msgnr) {
    try {
      // todo, sql injection?
      const select = `SELECT * FROM t100 WHERE sprsl='E' AND arbgb='${arbgb}' AND msgnr='${msgnr}' LIMIT 1`;
      const {result} = db.select({select});
      if (result[0]) {
        // todo, refactor this,
        text = result[0].values[0][3] as string;
      }
    } catch {
      // use fallback text
    }
  }

  if (text === undefined) {
    // fallback
    text = arbgb + ":" + msgnr + " &1 &2 &3 &4";
  }

  return text;
}

export class MessageStatement {
  private readonly context: Context;

  public constructor(context: Context) {
    this.context = context;
  }

  public message(options: IMessageOptions): void {
    let arbgb = options.id;
    if (arbgb !== undefined && typeof arbgb !== "string") {
      arbgb = arbgb.get();
    }
    arbgb = arbgb?.toUpperCase();
    // @ts-ignore
    abap.builtin.sy.get().msgid.set(arbgb);
    let msgnr = options.number;
    if (msgnr !== undefined && typeof msgnr !== "string") {
      msgnr = msgnr.get();
    }
    // @ts-ignore
    abap.builtin.sy.get().msgno.set(msgnr);

    const text = findText(this.context, arbgb, msgnr);

    const replaced = replace(text, options.with);

    if (options.into) {
      options.into.set(replaced);
    }
  }

}