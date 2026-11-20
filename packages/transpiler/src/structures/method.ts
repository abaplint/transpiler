import * as abaplint from "@abaplint/core";
import {IStructureTranspiler} from "./_structure_transpiler";
import {Traversal} from "../traversal";
import {Chunk} from "../chunk";
import {InlineDeclarations} from "../inline";
import {ClassConstructors} from "../class_constructors";

export class MethodTranspiler implements IStructureTranspiler {

  public transpile(node: abaplint.Nodes.StructureNode, traversal: Traversal): Chunk {
    const ret = new Chunk();

    for (const c of node.getChildren()) {
      ret.appendChunk(traversal.traverse(c));

      if (c.get() instanceof abaplint.Statements.MethodImplementation && c instanceof abaplint.Nodes.StatementNode) {
        const declare = InlineDeclarations.buildDeclarations(node, traversal);
        ret.appendString(declare);
        ret.appendString(this.classConstructorChecks(c, node, traversal));
      }
    }

    return ret;
  }

  /** a static method runs the class constructors of its class, on entry also the ones of the
   *  classes whose static attributes the method uses */
  private classConstructorChecks(statement: abaplint.Nodes.StatementNode, node: abaplint.Nodes.StructureNode,
                                 traversal: Traversal): string {
    const token = statement.findFirstExpression(abaplint.Expressions.MethodName)!.getFirstToken();
    const methodName = token.getStr();
    const scope = traversal.findCurrentScopeByToken(token);
    const classDef = traversal.getClassDefinition(token);
    let ret = "";
    if (methodName.toUpperCase() !== "CLASS_CONSTRUCTOR" && methodName.includes("~") === false) {
      const method = classDef?.getMethodDefinitions()?.getByName(methodName);
      if (method?.isStatic() === true && ClassConstructors.inChain(classDef, traversal, scope)) {
        const className = scope?.getParent()?.getIdentifier().sname?.toLowerCase();
        ret += ClassConstructors.check(Traversal.escapeNamespace(className)!);
      }
    }
    // while a method of the class runs, the class constructors of the class and its super classes have run
    return ret + ClassConstructors.entryChecks(node, traversal, ClassConstructors.chainNames(classDef, traversal, scope));
  }

}
