import * as abaplint from "@abaplint/core";
import {IStructureTranspiler} from "./_structure_transpiler";
import {Traversal} from "../traversal";
import {Chunk} from "../chunk";

export class ClassDefinitionTranspiler implements IStructureTranspiler {

  public transpile(node: abaplint.Nodes.StructureNode, traversal: Traversal): Chunk {
    const className = node.findFirstExpression(abaplint.Expressions.ClassName)?.concatTokens().toUpperCase();

    let found = false;
    if (className !== undefined) {
      for (const a of traversal.getCurrentObject().getABAPFiles()) {
        if (a.getInfo().getClassImplementationByName(className) !== undefined) {
          found = true;
        }
      }
    }

    if (found) {
      return new Chunk("");
    } else {
// its an abstract class with only abstract methods
      return new Chunk(`
class ${className?.toLowerCase()} {
  static INTERNAL_TYPE = 'CLAS';
  static IMPLEMENTED_INTERFACES = [];
  static INTERNAL_NAME = 'ABSTRACT_CLASS_INTERNAL_NAME';
  static ATTRIBUTES = {};
  static FRIENDS_ACCESS_STATIC = {};
  async constructor_() {
    this.me = new abap.types.ABAPObject();
    this.me.set(this);
    return this;
  }
}`);
    }
  }

}