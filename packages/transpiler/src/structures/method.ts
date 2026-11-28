import * as abaplint from "@abaplint/core";
import {IStructureTranspiler} from "./_structure_transpiler";
import {Traversal} from "../traversal";
import {Chunk} from "../chunk";
import {InlineDeclarations} from "../inline";
import {MethodImplementationTranspiler} from "../statements/method_implementation";

export class MethodTranspiler implements IStructureTranspiler {

  public transpile(node: abaplint.Nodes.StructureNode, traversal: Traversal): Chunk {
    const ret = new Chunk();

    // the constructor of a non-FINAL class records its class in the instance while it runs,
    // both halves of the try/finally are emitted here, so the braces always match
    const token = node.findDirectStatement(abaplint.Statements.MethodImplementation)
      ?.findFirstExpression(abaplint.Expressions.MethodName)?.getFirstToken();
    const constructing = this.findConstructingClass(token, traversal);

    for (const c of node.getChildren()) {
      if (constructing !== undefined && c.get() instanceof abaplint.Statements.EndMethod) {
        ret.appendString(MethodImplementationTranspiler.CONSTRUCTING_END);
      }

      ret.appendChunk(traversal.traverse(c));

      if (c.get() instanceof abaplint.Statements.MethodImplementation) {
        if (constructing !== undefined && token !== undefined) {
          ret.appendString(MethodImplementationTranspiler.buildConstructingStart(constructing, traversal, token));
        }
        const declare = InlineDeclarations.buildDeclarations(node, traversal);
        ret.appendString(declare);
      }
    }

    return ret;
  }

  private findConstructingClass(token: abaplint.Token | undefined, traversal: Traversal): abaplint.IClassDefinition | undefined {
    if (token === undefined || token.getStr().toUpperCase() !== "CONSTRUCTOR") {
      return undefined;
    }
    const cdef = traversal.getClassDefinition(token);
    return MethodImplementationTranspiler.tracksConstruction(cdef) ? cdef : undefined;
  }

}
