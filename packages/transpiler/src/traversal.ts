import * as abaplint from "@abaplint/core";
import * as StatementTranspilers from "./statements";
import * as ExpressionTranspilers from "./expressions";
import * as StructureTranspilers from "./structures";
import {IStatementTranspiler} from "./statements/_statement_transpiler";
import {IExpressionTranspiler} from "./expressions/_expression_transpiler";
import {IStructureTranspiler} from "./structures/_structure_transpiler";
import {TranspileTypes} from "./types";


export class Traversal {
  private readonly spaghetti: abaplint.ISpaghettiScope;
  private readonly file: abaplint.ABAPFile;

  public constructor(spaghetti: abaplint.ISpaghettiScope, file: abaplint.ABAPFile, _obj: abaplint.ABAPObject) {
    this.spaghetti = spaghetti;
    this.file = file;
  }

  public traverse(node: abaplint.INode | undefined): string {
    if (node instanceof abaplint.Nodes.StructureNode) {
      return this.traverseStructure(node);
    } else if (node instanceof abaplint.Nodes.StatementNode) {
      return this.traverseStatement(node);
    } else if (node instanceof abaplint.Nodes.ExpressionNode) {
      return this.traverseExpression(node);
    } else if (node === undefined) {
      throw new Error("Traverse, node undefined");
    } else {
      throw new Error("Traverse, unexpected node type");
    }
  }

  public getFilename(): string {
    return this.file.getFilename();
  }

  public getSpaghetti(): abaplint.ISpaghettiScope {
    return this.spaghetti;
  }

  public findCurrentScope(token: abaplint.Token) {
    return this.spaghetti.lookupPosition(token.getStart(), this.file.getFilename());
  }

  public getClassDefinition(token: abaplint.Token): abaplint.IClassDefinition | undefined {
    let scope = this.spaghetti.lookupPosition(token.getStart(), this.file.getFilename());

    while (scope !== undefined) {
      if (scope.getIdentifier().stype === abaplint.ScopeType.ClassImplementation
          || scope.getIdentifier().stype === abaplint.ScopeType.ClassDefinition) {

        return scope.findClassDefinition(scope?.getIdentifier().sname);
      }
      scope = scope.getParent();
    }

    return undefined;
  }

  public isClassAttribute(token: abaplint.Token): boolean {
    const scope = this.spaghetti.lookupPosition(token.getStart(), this.file.getFilename());
    if (scope === undefined) {
      throw new Error("isClassAttribute, unable to lookup position");
    }

    const name = token.getStr();
    const found = scope.findScopeForVariable(name);
    if (found && found.stype === abaplint.ScopeType.ClassImplementation) {
      return true;
    }
    return false;
  }

  public isStaticClassAttribute(token: abaplint.Token): string | undefined {
    const scope = this.spaghetti.lookupPosition(token.getStart(), this.file.getFilename());
    if (scope === undefined) {
      throw new Error("isStaticClassAttribute, unable to lookup position");
    }

    const name = token.getStr();
    const found = scope.findScopeForVariable(name);
    const id = scope.findVariable(name);
    if (found && id
        && id.getMeta().includes(abaplint.IdentifierMeta.Static)
        && found.stype === abaplint.ScopeType.ClassImplementation) {
      return scope.getParent()?.getIdentifier().sname;
    }
    return undefined;
  }

  public isBuiltin(token: abaplint.Token): boolean {
    const scope = this.spaghetti.lookupPosition(token.getStart(), this.file.getFilename());
    if (scope === undefined) {
      throw new Error("isBuiltin, unable to lookup position");
    }

    const name = token.getStr();
    const found = scope.findScopeForVariable(name);
    if (found && found.stype === abaplint.ScopeType.BuiltIn) {
      return true;
    }
    return false;
  }

  public buildConstructorContents(scope: abaplint.ISpaghettiScopeNode | undefined): string {
    const vars = scope?.getData().vars;
    if (vars === undefined || vars.length === 0) {
      return "";
    }
    let ret = "";
    for (const v of vars) {
      if (v.identifier.getMeta().includes(abaplint.IdentifierMeta.Static) === true) {
        continue;
      }
      ret += "this." + v.name + " = " + new TranspileTypes().toType(v.identifier.getType()) + ";\n";
    }
    return ret;
  }

  public determineType(node: abaplint.Nodes.ExpressionNode | abaplint.Nodes.StatementNode,
                       scope: abaplint.ISpaghettiScopeNode): abaplint.AbstractType | undefined {
    const found = node.findDirectExpression(abaplint.Expressions.Target);
    if (found === undefined) {
      return undefined;
    }

    const v = scope.findVariable(found.concatTokens());
    if (v) {
      return v.getType();
    }
    return undefined;
  }

////////////////////////////

  protected traverseStructure(node: abaplint.Nodes.StructureNode): string {
    const list: any = StructureTranspilers;
    let ret = "";

    for (const c of node.getChildren()) {
      if (c instanceof abaplint.Nodes.StructureNode) {
        const search = c.get().constructor.name + "Transpiler";
        if (list[search]) {
          const transpiler = new list[search]() as IStructureTranspiler;
          ret = ret + transpiler.transpile(c, this);
          continue;
        }
        ret = ret + this.traverseStructure(c);
      } else if (c instanceof abaplint.Nodes.StatementNode) {
        ret = ret + this.traverseStatement(c);
      } else {
        throw new Error("traverseStructure, unexpected child node type");
      }
    }
    return ret;
  }

  protected traverseStatement(node: abaplint.Nodes.StatementNode): string {
    const list: any = StatementTranspilers;
    const search = node.get().constructor.name + "Transpiler";
    if (list[search]) {
      const transpiler = new list[search]() as IStatementTranspiler;
      return transpiler.transpile(node, this) + "\n";
    }
    throw new Error(`Statement ${node.get().constructor.name} not supported, ${node.concatTokens()}`);
  }

  protected traverseExpression(node: abaplint.Nodes.ExpressionNode): string {
    const list: any = ExpressionTranspilers;
    const search = node.get().constructor.name + "Transpiler";
    if (list[search]) {
      const transpiler = new list[search]() as IExpressionTranspiler;
      return transpiler.transpile(node, this);
    }
    throw new Error(`Expression ${node.get().constructor.name} not supported, ${node.concatTokens()}`);
  }

}