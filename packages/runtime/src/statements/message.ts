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

async function findText(context: Context, arbgb: string | undefined, msgnr: string | undefined) {
  let text: string | undefined = undefined;

  if (arbgb && msgnr) {
    try {
      // todo, sql injection?
      const select = `SELECT * FROM t100 WHERE sprsl='E' AND arbgb='${arbgb}' AND msgnr='${msgnr}' LIMIT 1`;
      const {rows: result} = await context.defaultDB().select({select});
      if (result[0]) {
        text = result[0]["text"] as string;
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

  public async message(options: IMessageOptions) {
    let arbgb = options.id;
    if (arbgb !== undefined && typeof arbgb !== "string") {
      arbgb = arbgb.get();
    }
    arbgb = arbgb?.toUpperCase();

    let msgty = options.type;
    if (msgty !== undefined && typeof msgty !== "string") {
      msgty = msgty.get();
    }
    msgty = msgty?.toUpperCase();

    // @ts-ignore
    abap.builtin.sy.get().msgid.set(arbgb);
    let msgnr = options.number;
    if (msgnr !== undefined && typeof msgnr !== "string") {
      msgnr = msgnr.get();
    }
    // @ts-ignore
    abap.builtin.sy.get().msgno.set(msgnr);
    // @ts-ignore
    abap.builtin.sy.get().msgty.set(msgty);

    const text = await findText(this.context, arbgb, msgnr);

    const replaced = replace(text, options.with);

    if (options.into) {
      options.into.set(replaced);
    }
  }

}