import {ICharacter} from "../types/_character";
import {Table, String} from "../types";

export interface ISplitOptions {
  source: ICharacter | string,
  at: ICharacter | string,
  table?: Table,
  targets?: ICharacter[],
}

export function split(param: ISplitOptions) {
  const source = typeof param.source === "string" ? param.source : param.source.get();
  const at = typeof param.at === "string" ? param.at : param.at.get();

  const split = source.includes(at) ? source.split(at) : [];

  if (source.endsWith(at)) {
    split.pop();
  }

  if (param.table) {
    param.table.clear();
    for(const s of split) {
      param.table.append(new String().set(s));
    }
    if (source !== "" && split.length === 0) {
      param.table.append(new String().set(source));
    }
  }

  if (param.targets) {
    if (split.length === 0) {
      split.push(source);
    }
    for (const t of param.targets) {
      t.clear();
      if (split.length > 0) {
        t.set(split.shift()!.trimEnd());
      }
    }
    if (split.length > 0) {
      const concat = split.join(at);
      const last = param.targets[param.targets.length - 1];
      last.set(last.get() + at + concat);
    }
  }

}