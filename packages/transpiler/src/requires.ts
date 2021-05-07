import * as abaplint from "@abaplint/core";
import {IRequire} from ".";

export class Requires {
  private readonly reg: abaplint.IRegistry;

  public constructor(reg: abaplint.IRegistry) {
    this.reg = reg;
  }

  // todo, refactor this method
  public find(obj: abaplint.ABAPObject, _node: abaplint.ISpaghettiScopeNode, filename: string): readonly IRequire[] {
    const ret: IRequire[] = [];

    if (obj.getType() === "INTF") {
      return [];
    }

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

// add the superclass
    if (obj.getType() === "CLAS") {
      const clas = obj as abaplint.Objects.Class;
      const sup = clas.getDefinition()?.getSuperClass()?.toLowerCase();
      if (sup) {
        add({filename: sup + ".clas.abap", name: sup});
      }
      for (const f of clas.getSequencedFiles()) {
        if (f.getFilename() === filename
            || f.getFilename().endsWith(".clas.testclasses.abap")
            || f.getFilename() === clas.getMainABAPFile()?.getFilename()) {
          continue;
        }
        add({filename: f.getFilename(), name: undefined});
      }
    }

// this finds all OO references
/*
    for (const v of node.getData().references) {
      // todo, use the enum from abaplint, when its exported
      if (v.referenceType === "ObjectOrientedReference"
          && v.position.getFilename() === filename
          && v.resolved) {
        add({filename: v.resolved.getFilename(), name: v.resolved.getName().toLowerCase()});
      }
    }

    for (const c of node.getChildren()) {
      for (const f of this.find(obj, c, filename)) {
        add(f);
      }
    }
*/

    // always add CX_ROOT, it is used for CATCH, no catches in global interfaces
    if (obj.getType() !== "INTF") {
      const cx = this.reg.getObject("CLAS", "CX_ROOT");
      if (cx && cx instanceof abaplint.ABAPObject) {
        const main = cx.getMainABAPFile()?.getFilename();
        if (main) {
          add({filename: main, name: cx.getName().toLowerCase()});
        }
      }
    }

    return ret;
  }

}