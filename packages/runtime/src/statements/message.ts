import {ICharacter} from "../types/_character";
// import {SqlJs} from "sql.js/module"; todo

export interface IMessageOptions {
  id?: ICharacter | string,
  number?: ICharacter | string,
  type?: ICharacter | string,
  with?: (ICharacter | string)[],
  into?: ICharacter,
}

export function message(options: IMessageOptions): void {
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

  const text = findText.bind(this)(arbgb, msgnr);

  const replaced = replace(text, options.with);

  if (options.into) {
    options.into.set(replaced);
  }
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

    const field = "msgv" + ( i + 1 );
    if (i <= 3) {
      // @ts-ignore
      abap.builtin.sy.get()[field].set(replace);
    }

    text = text.replace(search, replace);
  }
  return text.trim();
}

function findText(arbgb: string | undefined, msgnr: string | undefined): string {
  let text: string | undefined = undefined;
  const db = this.db as any | undefined;
  if (db && arbgb && msgnr) {
    try {
      const stmt = db.prepare("SELECT * FROM t100 WHERE sprsl=:sprsl AND arbgb=:arbgb AND msgnr=:msgnr LIMIT 1");
      const result = stmt.getAsObject({
        ":sprsl": "E",
        ":arbgb": arbgb,
        ":msgnr": msgnr,
      });
      if (result.text) {
        text = result.text as string;
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