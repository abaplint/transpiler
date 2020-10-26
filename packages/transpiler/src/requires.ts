import * as abaplint from "@abaplint/core";
import {IObjectIdentifier} from ".";

export class Requires {
  private readonly reg: abaplint.IRegistry;
  private readonly obj: abaplint.ABAPObject;
  private readonly main: string | undefined;

  public constructor(reg: abaplint.IRegistry, obj: abaplint.ABAPObject) {
    this.reg = reg;
    this.obj = obj;
    this.main = obj.getMainABAPFile()?.getFilename();
  }

  public find(node: abaplint.ISpaghettiScopeNode, filename: string): readonly IObjectIdentifier[] {
    const ret: IObjectIdentifier[] = [];

    const add = function (obj: IObjectIdentifier | undefined) {
      if (obj === undefined) {
        return;
      }
      // skip if already in the list
      for (const r of ret) {
        if (r.type === obj.type && r.name === obj.name) {
          return;
        }
      }
      ret.push(obj);
    };

// this finds all OO references
    for (const v of node.getData().references) {
      // todo, use the enum from abaplint, when its exported
      if (v.referenceType === "ObjectOrientedReference"
          && v.resolved) {
        add(this.lookup(v.resolved.getName(), filename));
      }
    }

    for (const c of node.getChildren()) {
      for (const f of this.find(c, filename)) {
        add(f);
      }
    }

    return ret;
  }

//////////////////////////

  private lookup(name: string, filename: string): IObjectIdentifier | undefined {
    if (name.toUpperCase() === this.obj.getName().toUpperCase()
        && filename === this.main) {
      return undefined;
    }
    const found = this.reg.getObject("CLAS", name);
    if (found) {
      return {type: found.getType(), name: found.getName()};
    }
    return undefined;
  }

}