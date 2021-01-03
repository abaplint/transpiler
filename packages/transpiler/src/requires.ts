import * as abaplint from "@abaplint/core";
import {IRequire} from ".";

export class Requires {
  private readonly reg: abaplint.IRegistry;
  private readonly obj: abaplint.ABAPObject;

  public constructor(reg: abaplint.IRegistry, obj: abaplint.ABAPObject) {
    this.reg = reg;
    this.obj = obj;
  }

  public find(node: abaplint.ISpaghettiScopeNode, filename: string): readonly IRequire[] {
    const ret: IRequire[] = [];

    const add = function (req: IRequire | undefined) {
      if (req === undefined || req.filename === filename) {
        return;
      }
      // skip if already in the list
      for (const r of ret) {
        if (r.filename === req.filename && r.name === req.name) {
          return;
        }
      }
      ret.push(req);
    };

// this finds all OO references
    for (const v of node.getData().references) {
      // todo, use the enum from abaplint, when its exported
      if (v.referenceType === "ObjectOrientedReference"
          && v.resolved) {
        add({filename: v.resolved.getFilename(), name: v.resolved.getName()});
      }
    }

    for (const c of node.getChildren()) {
      for (const f of this.find(c, filename)) {
        add(f);
      }
    }

    // always add CX_ROOT, it is used for CATCH
    const cx = this.reg.getObject("CLAS", "CX_ROOT");
    if (cx && this.obj.getName().toUpperCase() !== "CX_ROOT" && cx instanceof abaplint.ABAPObject) {
      add({filename: cx.getMainABAPFile()!.getFilename(), name: cx.getName()});
    }

    return ret;
  }

}