import {Nodes, INode, ScopeType, Token, ABAPObject, ISpaghettiScope, ABAPFile, ISpaghettiScopeNode} from "@abaplint/core";
import * as StatementTranspilers from "./statements";
import * as ExpressionTranspilers from "./expressions";
import * as StructureTranspilers from "./structures";
import {IStatementTranspiler} from "./statements/_statement_transpiler";
import {IExpressionTranspiler} from "./expressions/_expression_transpiler";
import {IStructureTranspiler} from "./structures/_structure_transpiler";
import {TranspileTypes} from "./types";
import {IClassDefinition} from "@abaplint/core/build/src/abap/types/_class_definition";


export class Traversal {
  private readonly spaghetti: ISpaghettiScope;
  private readonly file: ABAPFile;

  public constructor(spaghetti: ISpaghettiScope, file: ABAPFile, _obj: ABAPObject) {
    this.spaghetti = spaghetti;
    this.file = file;
  }

  public traverse(node: INode | undefined): string {
    if (node instanceof Nodes.StructureNode) {
      return this.traverseStructure(node);
    } else if (node instanceof Nodes.StatementNode) {
      return this.traverseStatement(node);
    } else if (node instanceof Nodes.ExpressionNode) {
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

  public getSpaghetti(): ISpaghettiScope {
    return this.spaghetti;
  }

  public getClassDefinition(token: Token): IClassDefinition | undefined {
    let scope = this.spaghetti.lookupPosition(token.getStart(), this.file.getFilename());

    while (scope !== undefined) {
      if (scope.getIdentifier().stype === ScopeType.ClassImplementation
          || scope.getIdentifier().stype === ScopeType.ClassDefinition) {

        return scope.findClassDefinition(scope?.getIdentifier().sname);

//        return this.obj.getClassDefinition(scope?.getIdentifier().sname);
      }
      scope = scope.getParent();
    }

    return undefined;
  }

  public isClassAttribute(token: Token): boolean {
    const scope = this.spaghetti.lookupPosition(token.getStart(), this.file.getFilename());
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

  public isBuiltin(token: Token): boolean {
    const scope = this.spaghetti.lookupPosition(token.getStart(), this.file.getFilename());
    if (scope === undefined) {
      throw new Error("isBuiltin, unable to lookup position");
    }

    const name = token.getStr();
    const found = scope.findScopeForVariable(name);
    if (found && found.stype === ScopeType.BuiltIn) {
      return true;
    }
    return false;
  }

  public buildConstructorContents(scope: ISpaghettiScopeNode | undefined): string {
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