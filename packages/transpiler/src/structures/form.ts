import * as abaplint from "@abaplint/core";
import {IStructureTranspiler} from "./_structure_transpiler";
import {Traversal} from "../traversal";
import {Chunk} from "../chunk";
import {InlineDeclarations} from "../inline";
import {LocalTranspiler} from "../statements/local";

export class FormTranspiler implements IStructureTranspiler {

  public transpile(node: abaplint.Nodes.StructureNode, traversal: Traversal): Chunk {
    const formName = node.findFirstStatement(abaplint.Statements.Form)
      ?.findDirectExpression(abaplint.Expressions.FormName)?.concatTokens().toUpperCase();

    const ret = new Chunk();
    const local = LocalTranspiler.wrap(node);

    for (const c of node.getChildren()) {
      if (local && c.get() instanceof abaplint.Statements.EndForm) {
        ret.appendString(local.close);
      }
      ret.appendChunk(traversal.traverse(c));

      if (c.get() instanceof abaplint.Statements.Form) {
        const declare = InlineDeclarations.buildDeclarations(node, traversal);
        ret.appendString(declare);
        if (local) {
          ret.appendString(local.open);
        }
      }
    }

    if (formName && traversal.getCurrentObject().getType() === "PROG") {
      ret.appendString(`abap.Forms['PROG-${traversal.getCurrentObject().getName().toUpperCase()
        }-${formName}'] = ${Traversal.formName(formName)};`);
    }

    return ret;
  }

}