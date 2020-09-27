import * as abaplint from "@abaplint/core";
import {IObjectIdentifier} from ".";

export class Requires {
  private readonly reg: abaplint.IRegistry;
  private readonly obj: abaplint.ABAPObject;

  public constructor(reg: abaplint.IRegistry, obj: abaplint.ABAPObject) {
    this.reg = reg;
    this.obj = obj;
  }

  public find(node: abaplint.ISpaghettiScopeNode): readonly IObjectIdentifier[] {
    const ret: IObjectIdentifier[] = [];

    const add = function (obj: IObjectIdentifier | undefined) {
      if (obj === undefined) {
        return;
      }
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
      if (v.referenceType === "ObjectOrientedReference") {
        add(this.lookup(v.resolved.getName()));
      }
    }

    for (const c of node.getChildren()) {
      for (const f of this.find(c)) {
        add(f);
      }
    }

    return ret;
  }

//////////////////////////

  private lookup(name: string): IObjectIdentifier | undefined {
    if (name.toUpperCase() === this.obj.getName().toUpperCase()) {
      return undefined;
    }
    const found = this.reg.getObject("CLAS", name);
    if (found) {
      return {type: found.getType(), name: found.getName()};
    }
    return undefined;
  }

}