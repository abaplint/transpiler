import * as abaplint from "@abaplint/core";
import {IObjectIdentifier} from ".";

export class Requires {
  private readonly reg: abaplint.IRegistry;

  public constructor(reg: abaplint.IRegistry) {
    this.reg = reg;
  }

  public find(node: abaplint.ISpaghettiScopeNode): readonly IObjectIdentifier[] {
    let ret: IObjectIdentifier[] = [];

// this finds all OO instance variables
/*
    for (const v of node.getData().vars) {
      const type = v.identifier.getType();
      if (v.identifier.getName() !== "me" // todo, this is a hack
          && type instanceof abaplint.BasicTypes.ObjectReferenceType) {
        const found = this.lookup(type.getName());
        if (found) {
          ret.push({type: found.getType(), name: found.getName()});
        }
      }
    }
    */

// this finds all OO references
    for (const v of node.getData().references) {
      // todo, use the enum
      if (v.referenceType === "ObjectOrientedReference") {
        const found = this.lookup(v.resolved.getName());
        if (found) {
          ret.push({type: found.getType(), name: found.getName()});
        }
      }
    }

    for (const c of node.getChildren()) {
      ret = ret.concat(this.find(c));
    }

// todo, duplicates?

    return ret;
  }

  private lookup(name: string) {
    const found = this.reg.getObject("CLAS", name);
    return found;
  }

}