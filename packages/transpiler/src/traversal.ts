import * as abaplint from "@abaplint/core";
import * as StatementTranspilers from "./statements";
import * as ExpressionTranspilers from "./expressions";
import * as StructureTranspilers from "./structures";
import {IStatementTranspiler} from "./statements/_statement_transpiler";
import {IExpressionTranspiler} from "./expressions/_expression_transpiler";
import {IStructureTranspiler} from "./structures/_structure_transpiler";
import {TranspileTypes} from "./types";
import {ISpaghettiScopeNode} from "@abaplint/core";


export class Traversal {
  private readonly spaghetti: abaplint.ISpaghettiScope;
  private readonly file: abaplint.ABAPFile;
  private readonly obj: abaplint.ABAPObject;
  private readonly reg: abaplint.IRegistry;

  public constructor(spaghetti: abaplint.ISpaghettiScope, file: abaplint.ABAPFile, obj: abaplint.ABAPObject, reg: abaplint.IRegistry) {
    this.spaghetti = spaghetti;
    this.file = file;
    this.obj = obj;
    this.reg = reg;
  }

  public getCurrentObject(): abaplint.ABAPObject {
    return this.obj;
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

  private scopeCache: {
    cov: {start: abaplint.Position, end: abaplint.Position},
    filename: string,
    node: abaplint.ISpaghettiScopeNode
  } | undefined = undefined;

  public findCurrentScope(token: abaplint.Token) {
    const filename = this.file.getFilename();

    if (this.scopeCache
      && this.scopeCache.filename === filename
      && token.getEnd().isBetween(this.scopeCache.cov.start, this.scopeCache.cov.end)) {
      return this.scopeCache.node;
    }

    const node = this.spaghetti.lookupPosition(token.getStart(), filename);

// note: cache only works for leafs, as parent nodes cover multiple leaves
    if (node && node.getChildren().length === 0) {
      this.scopeCache = {
        cov: node.calcCoverage(),
        filename: filename,
        node: node,
      };
    } else {
      this.scopeCache = undefined;
    }

    return node;
  }

  public getClassDefinition(token: abaplint.Token): abaplint.IClassDefinition | undefined {
    let scope = this.findCurrentScope(token);

    while (scope !== undefined) {
      if (scope.getIdentifier().stype === abaplint.ScopeType.ClassImplementation
          || scope.getIdentifier().stype === abaplint.ScopeType.ClassDefinition) {

        return scope.findClassDefinition(scope?.getIdentifier().sname);
      }
      scope = scope.getParent();
    }

    return undefined;
  }

  private isClassAttribute(token: abaplint.Token): boolean {
    const scope = this.findCurrentScope(token);
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

  public findPrefix(t: abaplint.Token): string {
    let name = t.getStr().toLowerCase();
    const cla = this.isStaticClassAttribute(t);
    if (cla) {
      name = cla + "." + name;
    } else if (name === "super") {
      return name;
    } else if (this.isClassAttribute(t)) {
      name = "this." + name;
    } else if (this.isBuiltinVariable(t)) {
      name = "abap.builtin." + name;
    }
    return name;
  }

  private isStaticClassAttribute(token: abaplint.Token): string | undefined {
    const scope = this.findCurrentScope(token);
    if (scope === undefined) {
      throw new Error("isStaticClassAttribute, unable to lookup position");
    }

    const name = token.getStr();
    const found = scope.findScopeForVariable(name);
    const id = scope.findVariable(name);
    if (found && id
        && id.getMeta().includes(abaplint.IdentifierMeta.Static)
        && found.stype === abaplint.ScopeType.ClassImplementation) {
      return scope.getParent()?.getIdentifier().sname.toLowerCase();
    }
    return undefined;
  }

  public isBuiltinMethod(token: abaplint.Token): boolean {
    const scope = this.findCurrentScope(token);
    if (scope === undefined) {
      return false;
    }

    for (const r of scope.getData().references) {
      if (r.referenceType === abaplint.ReferenceType.BuiltinMethodReference
          && r.position.getStart().equals(token.getStart())) {
        return true;
      }
    }
    return false;
  }

  public findMethodReference(token: abaplint.Token, scope: ISpaghettiScopeNode | undefined):
  undefined | {def: abaplint.Types.MethodDefinition, name: string} {

    if (scope === undefined) {
      return undefined;
    }

    for (const r of scope.getData().references) {
      if (r.referenceType === abaplint.ReferenceType.MethodReference
          && r.position.getStart().equals(token.getStart())
          && r.resolved instanceof abaplint.Types.MethodDefinition) {
        let name = r.resolved.getName();
        if (r.extra?.ooName && r.extra?.ooType === "INTF") {
          name = r.extra.ooName + "$" + name;
        }

        return {def: r.resolved, name};
      } else if (r.referenceType === abaplint.ReferenceType.BuiltinMethodReference
          && r.position.getStart().equals(token.getStart())) {
        const def = r.resolved as abaplint.Types.MethodDefinition;
        const name = def.getName();

        return {def, name};
      }
    }

    return undefined;
  }

  private isBuiltinVariable(token: abaplint.Token): boolean {
    const scope = this.findCurrentScope(token);
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

  // returns the interface name if interfaced
  public isInterfaceAttribute(token: abaplint.Token): string | undefined {
    const ref = this.findReadOrWriteReference(token);
    if (ref === undefined) {
      return undefined;
    }

    // local classes
    const scope = this.findCurrentScope(ref.getToken());
    if (scope?.getIdentifier().stype === abaplint.ScopeType.Interface) {
      return scope?.getIdentifier().sname;
    }

    // global classes
    const file = this.reg.getFileByName(ref.getFilename());
    if (file) {
      const obj = this.reg.findObjectForFile(file);
      if (obj?.getType() === "INTF") {
        return obj.getName().toLowerCase();
      }
    }

    return undefined;
  }

  private findReadOrWriteReference(token: abaplint.Token) {
    const scope = this.findCurrentScope(token);
    if (scope === undefined) {
      return undefined;
    }

    for (const r of scope.getData().references) {
      if ((r.referenceType === abaplint.ReferenceType.DataReadReference
          || r.referenceType === abaplint.ReferenceType.DataWriteReference)
          && r.position.getStart().equals(token.getStart())) {
        return r.resolved;
      }
    }
    return undefined;
  }

  public buildConstructorContents(scope: abaplint.ISpaghettiScopeNode | undefined,
                                  def: abaplint.IClassDefinition, inputName: string): string {

    const vars = scope?.getData().vars;
    if (vars === undefined || vars.length === 0) {
      return "";
    }
    let ret = "";

    if (def.getSuperClass() !== undefined) {
      // todo, more here, there might be parameters to pass
      // for now just pass the same input
      ret += `await super.constructor_(${inputName});\n`;
    }

    for (const v of vars) {
      if (v.identifier.getMeta().includes(abaplint.IdentifierMeta.Static) === true) {
        continue;
      }
      const name = v.name.toLowerCase().replace("~", "$");
      if (name === "super") {
        continue; // todo, https://github.com/abaplint/transpiler/issues/133
      }

      // todo, better handling of variables from interfaces, it should only initialize those that are directly implemented
      if (def.getAttributes().findByName(name) === undefined
          && name.includes("$") === false
          && name !== "me") {
        continue;
      }

      ret += "this." + name + " = " + new TranspileTypes().toType(v.identifier.getType()) + ";\n";
      if (name === "me") {
        ret += "this.me.set(this);\n";
      }
    }
    return ret;
  }

  public determineType(node: abaplint.Nodes.ExpressionNode | abaplint.Nodes.StatementNode,
                       scope: abaplint.ISpaghettiScopeNode): abaplint.AbstractType | undefined {
    const found = node.findDirectExpression(abaplint.Expressions.Target);
    if (found === undefined) {
      return undefined;
    }

    let context: abaplint.AbstractType | undefined = undefined;
    for (const c of found.getChildren()) {
      if (context === undefined) {
        context = scope.findVariable(c.getFirstToken().getStr())?.getType();
      } else if (c.get() instanceof abaplint.Expressions.ComponentName
          && context instanceof abaplint.BasicTypes.StructureType) {
        context = context.getComponentByName(c.getFirstToken().getStr());
      }
    }

    return context;
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