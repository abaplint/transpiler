import * as abaplint from "@abaplint/core";
import {IStructureTranspiler} from "./_structure_transpiler";
import {Traversal} from "../traversal";
import {Chunk} from "../chunk";
import {filesWithProgramIncludes} from "../program_includes";
import {ClassImplementationTranspiler} from "./class_implementation";

export class ClassDefinitionTranspiler implements IStructureTranspiler {

  public transpile(node: abaplint.Nodes.StructureNode, traversal: Traversal): Chunk {
    const className = node.findFirstExpression(abaplint.Expressions.ClassName)?.concatTokens().toUpperCase();

    // all files of the object, not getSequencedFiles(): for function groups that only lists the main
    // program and the function modules, an IMPLEMENTATION in eg. the F01 include must also be found.
    // Programs additionally have their INCLUDE programs expanded into them
    const obj = traversal.getCurrentObject();
    const files = obj instanceof abaplint.Objects.Program
      ? filesWithProgramIncludes(obj, traversal.reg)
      : obj.getABAPFiles();
    const found = className !== undefined
      && files.some(a => a.getInfo().getClassImplementationByName(className) !== undefined);

    if (found) {
      return new Chunk("");
    } else {
      // no IMPLEMENTATION, eg. an exception class or an abstract class with only abstract methods
      return new ClassImplementationTranspiler().transpileWithoutImplementation(node, traversal);
    }
  }

}
