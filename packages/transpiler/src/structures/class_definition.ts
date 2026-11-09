import * as abaplint from "@abaplint/core";
import {IStructureTranspiler} from "./_structure_transpiler";
import {Traversal} from "../traversal";
import {Chunk} from "../chunk";
import {filesWithProgramIncludes} from "../program_includes";
import {ClassImplementationTranspiler} from "./class_implementation";

export class ClassDefinitionTranspiler implements IStructureTranspiler {

  public transpile(node: abaplint.Nodes.StructureNode, traversal: Traversal): Chunk {
    const className = node.findFirstExpression(abaplint.Expressions.ClassName)?.concatTokens().toUpperCase();

    let found = false;
    if (className !== undefined) {
      for (const a of filesWithProgramIncludes(traversal.getCurrentObject(), traversal.reg)) {
        if (a.getInfo().getClassImplementationByName(className) !== undefined) {
          found = true;
        }
      }
    }

    if (found) {
      return new Chunk("");
    } else {
      // no IMPLEMENTATION, eg. an exception class or an abstract class with only abstract methods
      return new ClassImplementationTranspiler().transpileWithoutImplementation(node, traversal);
    }
  }

}
