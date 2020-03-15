import {Nodes, SpaghettiScope, INode, ScopeType} from "abaplint";
import * as StatementTranspilers from "./statements";
import * as ExpressionTranspilers from "./expressions";
import * as StructureTranspilers from "./structures";
import {IStatementTranspiler} from "./statements/_statement_transpiler";
import {IExpressionTranspiler} from "./expressions/_expression_transpiler";
import {IStructureTranspiler} from "./structures/_structure_transpiler";
import {SpaghettiScopeNode} from "abaplint/build/src/abap/syntax/spaghetti_scope";
import {TranspileTypes} from "./types";
// import {Token} from "abaplint/build/src/abap/tokens/_token"; // todo, bad import

export class Traversal {
  private readonly spaghetti: SpaghettiScope;
  private readonly filename: string;

  public constructor(spaghetti: SpaghettiScope, filename: string) {
    this.spaghetti = spaghetti;
    this.filename = filename;
  }

  public traverse(node: INode | undefined): string {
    if (node instanceof Nodes.StructureNode) {
      return this.traverseStructure(node);
    } else if (node instanceof Nodes.StatementNode) {
      return this.traverseStatement(node);
    } else if (node instanceof Nodes.ExpressionNode) {
      return this.traverseExpression(node);
    } else {
      throw new Error("Traverse, unexpected node type");
    }
  }

  public getFilename(): string {
    return this.filename;
  }

  public getSpaghetti(): SpaghettiScope {
    return this.spaghetti;
  }

  public isClassAttribute(token: any): boolean {
    const scope = this.spaghetti.lookupPosition(token.getStart(), this.filename);
    if (scope === undefined) {
      throw new Error("isClassAttribute, unable to lookup position");
    }

    const name = token.getStr();
    const found = scope.findScopeForVariable(name);
    if (found && found.stype === ScopeType.ClassImplementation) {
      return true;
    }
    return false;
  }

  public buildConstructorContents(scope: SpaghettiScopeNode | undefined): string {
    const vars = scope?.getData().vars;
    if (vars === undefined || vars.length === 0) {
      return "";
    }
    let ret = "";
    for (const v of vars) {
      ret = ret + "this." + v.name + " = " + new TranspileTypes().toType(v.identifier.getType()) + ";\n";
    }
    return ret;
  }

  protected traverseStructure(node: Nodes.StructureNode): string {
    const list: any = StructureTranspilers;
    let ret = "";

    for (const c of node.getChildren()) {
      if (c instanceof Nodes.StructureNode) {
        const search = c.get().constructor.name + "Transpiler";
        if (list[search]) {
          const transpiler = new list[search]() as IStructureTranspiler;
          ret = ret + transpiler.transpile(c, this);
          continue;
        }
        ret = ret + this.traverseStructure(c);
      } else if (c instanceof Nodes.StatementNode) {
        ret = ret + this.traverseStatement(c);
      } else {
        throw new Error("traverseStructure, unexpected child node type");
      }
    }
    return ret;
  }

  protected traverseStatement(node: Nodes.StatementNode): string {
    const list: any = StatementTranspilers;
    const search = node.get().constructor.name + "Transpiler";
    if (list[search]) {
      const transpiler = new list[search]() as IStatementTranspiler;
      return transpiler.transpile(node, this) + "\n";
    }
    throw new Error(`Statement ${node.get().constructor.name} not supported`);
  }

  protected traverseExpression(node: Nodes.ExpressionNode): string {
    const list: any = ExpressionTranspilers;
    const search = node.get().constructor.name + "Transpiler";
    if (list[search]) {
      const transpiler = new list[search]() as IExpressionTranspiler;
      return transpiler.transpile(node, this);
    }
    throw new Error(`Expression ${node.get().constructor.name} not supported`);
  }

}