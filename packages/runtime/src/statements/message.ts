import {Context} from "../context";
import {ABAPObject, Character, String} from "../types";
import {ICharacter} from "../types/_character";
import {ABAP} from "..";

declare const abap: ABAP;

export interface IMessageOptions {
  id?: ICharacter | string,
  number?: ICharacter | string,
  type?: ICharacter | string,
  displayLike?: ICharacter | string,
  exceptionOrText?: ABAPObject | ICharacter | String,
  with?: (ICharacter | string)[],
  into?: ICharacter,
}

function replace(text: string, w?: (ICharacter | string | Character)[]): string {
  for (let i = 0; i < 6; i++) {
    const search = "&" + (i + 1);
    let replace = "";
    if (w && w[i]) {
      const j = w[i];
      if (typeof j === "string") {
        replace = j;
      } else {
// todo: some formatting missing here,
        replace = (j.get() + "").trimEnd();
      }
    }

    const field = "msgv" + (i + 1);
    if (i <= 3) {
      abap.builtin.sy.get()[field].set(replace);
    }

    text = text.replace(search, replace);
  }
  return text.trim();
}

async function findText(context: Context, arbgb: string | undefined, msgnr: string | undefined, msgty: string | undefined) {
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
    // @ts-ignore
    text = abap.MSAG[arbgb?.trimEnd().toUpperCase()]?.[msgnr];
  }

  if (text === undefined) {
    // fallback
    text = msgty + ":" + arbgb?.trim() + ":" + msgnr + " &1 &2 &3 &4";
  }

  return text;
}

/** a placeholder of IF_T100_MESSAGE: the content of the attribute ATTRn names, or the name itself framed by & when
 * the object has no such attribute; an empty name gives an empty value */
function t100Value(obj: any, attr: string): string {
  const name = attr.trimEnd();
  if (name === "") {
    return "";
  }
  const value = obj[name.toLowerCase().replace("~", "$").replace(/\//g, "$")];
  if (value === undefined || typeof value.get !== "function") {
    return "&" + name + "&";
  }
  const content = value.get();
  return typeof content === "object" ? "&" + name + "&" : (content + "").trimEnd();
}

export class MessageStatement {
  private readonly context: Context;

  public constructor(context: Context) {
    this.context = context;
  }

  /** returns the text of the message */
  public async message(options: IMessageOptions): Promise<string> {
    // MESSAGE oref, the object implements IF_T100_MESSAGE: its T100 key is the message
    const t100 = options.exceptionOrText instanceof ABAPObject
      ? options.exceptionOrText.get()?.if_t100_message$t100key?.get() : undefined;
    if (t100 !== undefined) {
      const obj = options.exceptionOrText!.get();
      const dynamicType = obj.if_t100_dyn_msg$msgty?.get();
      options = {...options,
        id: t100.msgid.get().trimEnd(),
        number: t100.msgno.get(),
        type: options.type ?? (dynamicType ? dynamicType : undefined),
        exceptionOrText: undefined,
        with: [t100.attr1, t100.attr2, t100.attr3, t100.attr4].map((a: any) => t100Value(obj, a.get())),
      };
    }

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

    abap.builtin.sy.get().msgid.set(arbgb || "");
    let msgnr = options.number;
    if (msgnr !== undefined && typeof msgnr !== "string") {
      msgnr = msgnr.get();
    }
    abap.builtin.sy.get().msgno.set(msgnr || "");
    abap.builtin.sy.get().msgty.set(msgty);

    let replaced = "";
    if (options.exceptionOrText) {
      if (options.exceptionOrText instanceof ABAPObject) {
        replaced = await options.exceptionOrText.get().if_message$get_text();
      } else {
        replaced = options.exceptionOrText.get();
      }
    } else {
      const text = await findText(this.context, arbgb, msgnr, msgty);
      replaced = replace(text, options.with);
    }

    if (options.into) {
      options.into.set(replaced);
    } else {
// hmm, add option on how/if to write messages to console? or it should be the abap.console() ?
      console.log(replaced);
    }
    return replaced;
  }

}