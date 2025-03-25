import * as abaplint from "@abaplint/core";
import * as StatementTranspilers from "./statements";
import * as ExpressionTranspilers from "./expressions";
import * as StructureTranspilers from "./structures";
import {IStatementTranspiler} from "./statements/_statement_transpiler";
import {IExpressionTranspiler} from "./expressions/_expression_transpiler";
import {IStructureTranspiler} from "./structures/_structure_transpiler";
import {TranspileTypes} from "./transpile_types";
import {ISpaghettiScopeNode} from "@abaplint/core";
import {Chunk} from "./chunk";
import {ConstantTranspiler} from "./expressions";
import {ITranspilerOptions} from "./types";
import {DEFAULT_KEYWORDS} from "./keywords";

export class Traversal {
  private readonly spaghetti: abaplint.ISpaghettiScope;
  private readonly file: abaplint.ABAPFile;
  private readonly obj: abaplint.ABAPObject;
  public readonly reg: abaplint.IRegistry;
  public readonly options: ITranspilerOptions | undefined;

  public constructor(spaghetti: abaplint.ISpaghettiScope, file: abaplint.ABAPFile,
                     obj: abaplint.ABAPObject, reg: abaplint.IRegistry, options?: ITranspilerOptions) {
    this.spaghetti = spaghetti;
    this.file = file;
    this.obj = obj;
    this.reg = reg;
    this.options = options;
  }

  public static escapeNamespace(name: string | undefined) {
    return name?.replace(/\//g, "$");
  }

  public static prefixVariable(name: string | undefined) {
    // TODO: performace, make this a hash lookup?,
    if (DEFAULT_KEYWORDS.some(k => k === name)) {
      return "$" + name;
    }
    return name + "";
  }

  public getCurrentObject(): abaplint.ABAPObject {
    return this.obj;
  }

  public traverse(node: abaplint.INode | undefined): Chunk {
    if (node instanceof abaplint.Nodes.StructureNode) {
      return this.traverseStructure(node);
    } else if (node instanceof abaplint.Nodes.StatementNode) {
      return this.traverseStatement(node);
    } else if (node instanceof abaplint.Nodes.ExpressionNode) {
      return this.traverseExpression(node);
    } else if (node === undefined) {
      // perhaps the file contains syntax errors? these are not reported for dependencies
      throw new Error("Traverse, node undefined, " + this.getFilename());
    } else {
      throw new Error("Traverse, unexpected node type");
    }
  }

  public getFilename(): string {
    return this.file.getFilename();
  }

  public getFile(): abaplint.ABAPFile {
    return this.file;
  }

  public getSpaghetti(): abaplint.ISpaghettiScope {
    return this.spaghetti;
  }

  /** finds a statement in the _current_ file given a position */
  public findStatementInFile(pos: abaplint.Position): abaplint.Nodes.StatementNode | undefined {
    for (const s of this.file.getStatements()) {
      if (pos.isBetween(s.getStart(), s.getEnd())) {
        return s;
      }
    }
    return undefined;
  }

  private scopeCache: {
    cov: {start: abaplint.Position, end: abaplint.Position},
    filename: string,
    node: abaplint.ISpaghettiScopeNode
  } | undefined = undefined;

  public findCurrentScopeByToken(token: abaplint.Token) {
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

    if (node === undefined
        && this.obj instanceof abaplint.Objects.FunctionGroup
        && this.obj.getInclude(filename.split(".")[2].replace(/#/g, "/"))) {
      // workaround for INCLUDEs in function groups
      return this.fuctionGroupWorkaround(token);
    }

    return node;
  }

  private fuctionGroupWorkaround(token: abaplint.Token): abaplint.ISpaghettiScopeNode | undefined {
    // check the function group level, it might be an INCLUDE defining the DATA
    const functionGroupLevel = this.spaghetti.getFirstChild()?.getFirstChild();

    const variable = functionGroupLevel?.getData().vars[token.getStr().toUpperCase()];
    if (variable?.getStart().equals(token.getStart())) {
      return functionGroupLevel;
    }

    return undefined;
  }

  public getInterfaceDefinition(token: abaplint.Token): abaplint.IInterfaceDefinition | undefined {
    let scope = this.findCurrentScopeByToken(token);

    while (scope !== undefined) {
      if (scope.getIdentifier().stype === abaplint.ScopeType.Interface) {
        return scope.findInterfaceDefinition(scope?.getIdentifier().sname);
      }
      scope = scope.getParent();
    }

    return undefined;
  }

  public getClassDefinition(token: abaplint.Token): abaplint.IClassDefinition | undefined {
    let scope = this.findCurrentScopeByToken(token);

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
    const scope = this.findCurrentScopeByToken(token);
    if (scope === undefined) {
      throw new Error("isClassAttribute, unable to lookup position");
    }

    const name = token.getStr();
    if (name.toLowerCase() === "me") {
      return true;
    }
    const found = scope.findScopeForVariable(name);
    if (found && (found.stype === abaplint.ScopeType.MethodInstance
        || found.stype === abaplint.ScopeType.ClassImplementation)) {
      return true;
    }
    return false;
  }

  public prefixAndName(t: abaplint.Token, filename?: string): string {
    let name = t.getStr().toLowerCase();

    if (filename && this.getCurrentObject().getABAPFileByName(filename) === undefined) {
      // the prefix is from a different object
      const file = this.reg.getFileByName(filename);
      if (file) {
        const found = this.reg.findObjectForFile(file);
        if (found) {
          if (found instanceof abaplint.Objects.Interface) {
            return Traversal.escapeNamespace(this.lookupClassOrInterface(found.getName(), t)) + "." + found.getName().toLowerCase() + "$" + name;
          } else {
            return Traversal.escapeNamespace(this.lookupClassOrInterface(found.getName(), t)) + "." + name;
          }
        }
      }
    }

    const className = this.isStaticClassAttribute(t);
    if (className) {
      name = Traversal.escapeNamespace(className) + "." + name;
    } else if (name === "super") {
      return name;
    } else if (this.isClassAttribute(t)) {
      name = "this." + Traversal.escapeNamespace(name);
    } else if (this.isBuiltinVariable(t)) {
      name = "abap.builtin." + name.toLowerCase().replace("%", "$");
    } else if (this.isTypePool(t)) {
      const tp = this.isTypePool(t);
      name = `abap.TypePools["${tp}"].` + name.toLowerCase();
    }
    return name;
  }

  private isStaticClassAttribute(token: abaplint.Token): string | undefined {
    const scope = this.findCurrentScopeByToken(token);
    if (scope === undefined) {
      throw new Error(`isStaticClassAttribute, unable to lookup position, ${token.getStr()},${token.getRow()},${token.getCol()},` +
        this.file.getFilename());
    }

    const name = token.getStr();
    const found = scope.findScopeForVariable(name);
    const id = scope.findVariable(name);
    if (found && id
        && id.getMeta().includes(abaplint.IdentifierMeta.Static)
        && found.stype === abaplint.ScopeType.ClassImplementation) {
//      console.dir(found.sname);
      return found.sname.toLowerCase();
    }
    return undefined;
  }

  public buildMethods(def: abaplint.IClassDefinition | abaplint.IInterfaceDefinition | undefined,
                      _scope: abaplint.ISpaghettiScopeNode | undefined): string[] {
    const methods: string[] = [];
    if (def === undefined) {
      return methods;
    }

    const methodDefinitions = def.getMethodDefinitions();
    if (methodDefinitions === undefined) {
      // this can occur if the dependencies is not 702?
      throw new Error("buildMethods: unexpected undefined, " + def.getName());
    }

    for (const m of methodDefinitions.getAll()) {
      const parameters: string[] = [];
      for (const p of m.getParameters().getAll()) {
        const type = new TranspileTypes().toType(p.getType());
        const optional = m.getParameters().getOptional().includes(p.getName()) ? "X" : " ";
        parameters.push(`"${p.getName().toUpperCase()}": {"type": () => {return ${type};}, "is_optional": "${optional}"}`);
      }

      methods.push(`"${m.getName().toUpperCase()}": {"visibility": "${
        this.mapVisibility(m.getVisibility())}", "parameters": {${
        parameters.join(", ")}}}`);
    }

    return methods;
  }

  private mapVisibility(vis: abaplint.Visibility) {
    switch (vis) {
      case abaplint.Visibility.Private:
        return "I";
      case abaplint.Visibility.Protected:
        return "O";
      default:
        return "U";
    }
  }

  public buildAttributes(def: abaplint.IClassDefinition | abaplint.IInterfaceDefinition | undefined,
                         scope: abaplint.ISpaghettiScopeNode | undefined,
                         prefix = ""): string[] {
    const attr: string[] = [];
    if (def === undefined) {
      return attr;
    }

    for (const a of def.getAttributes()?.getAll() || []) {
      const type = new TranspileTypes().toType(a.getType());
      const runtime = this.mapVisibility(a.getVisibility());
      const isClass = a.getMeta().includes(abaplint.IdentifierMeta.Static) ? "X" : " ";
      attr.push(`"${prefix + a.getName().toUpperCase()}": {"type": () => {return ${type};}, "visibility": "${
        runtime}", "is_constant": " ", "is_class": "${isClass}"}`);
    }

    for (const a of def.getAttributes()?.getConstants() || []) {
      const type = new TranspileTypes().toType(a.getType());
      let runtime = "";
      switch (a.getVisibility()) {
        case abaplint.Visibility.Private:
          runtime = "I";
          break;
        case abaplint.Visibility.Protected:
          runtime = "O";
          break;
        default:
          runtime = "U";
      }
      attr.push(`"${prefix + a.getName().toUpperCase()}": {"type": () => {return ${type};}, "visibility": "${
        runtime}", "is_constant": "X", "is_class": "X"}`);
    }

    for (const impl of def.getImplementing()) {
      const idef = this.findInterfaceDefinition(impl.name, scope);
      attr.push(...this.buildAttributes(idef, scope, impl.name.toUpperCase() + "~"));
    }

    return attr;
  }

  public isBuiltinMethod(token: abaplint.Token): boolean {
    const scope = this.findCurrentScopeByToken(token);
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

  public isSQLConversion(token: abaplint.Token): string | undefined {
    const scope = this.findCurrentScopeByToken(token);
    for (const s of scope?.getData().sqlConversion || []) {
      if (s.token.getStart().equals(token.getStart())) {
        return s.fieldName;
      }
    }
    return undefined;
  }

  public findMethodReference(token: abaplint.Token, scope: ISpaghettiScopeNode | undefined):
  undefined | {def: abaplint.Types.MethodDefinition, name: string} {
    let candidate: undefined | {def: abaplint.Types.MethodDefinition, name: string} = undefined;

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

        candidate = {def: r.resolved, name};
        if (token.getStart() instanceof abaplint.VirtualPosition
          && name.toLowerCase().includes(token.getStr().toLowerCase()) === false) {
          // if its macros and they are nested everything can go wrong, so try looking for a better candidate
          continue;
        }
        return candidate;
      } else if (r.referenceType === abaplint.ReferenceType.BuiltinMethodReference
          && r.position.getStart().equals(token.getStart())) {
        const def = r.resolved as abaplint.Types.MethodDefinition;
        const name = def.getName();

        return {def, name};
      }
    }

    return candidate;
  }

  private isBuiltinVariable(token: abaplint.Token): boolean {
    const scope = this.findCurrentScopeByToken(token);
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

  public isTypePool(token: abaplint.Token): string | undefined {
    const ref = this.findReadOrWriteReference(token);
    if (ref?.getFilename().endsWith(".type.abap")) {
      const file = this.reg.getFileByName(ref.getFilename());
      if (file === undefined) {
        return undefined;
      }
      const obj = this.reg.findObjectForFile(file);
      return obj?.getName();
    }
    return undefined;
  }

  // returns the interface name if interfaced
  public isInterfaceAttribute(token: abaplint.Token): string | undefined {
    const ref = this.findReadOrWriteReference(token);
    if (ref === undefined) {
      return undefined;
    }

    // local
    if (ref.getFilename() === this.getFilename()) {
      const scope = this.findCurrentScopeByToken(ref.getToken());
      if (scope?.getIdentifier().stype === abaplint.ScopeType.Interface) {
        return scope?.getIdentifier().sname;
      }
    }

    // global
    for (const obj of this.reg.getObjectsByType("INTF")) {
      if (obj.getFiles().some(f => f.getFilename() === ref.getFilename())) {
        return obj.getName().toLowerCase();
      }
    }
    /*
    const file = this.reg.getFileByName(ref.getFilename());
    if (file) {
      const obj = this.reg.findObjectForFile(file);
      if (obj?.getType() === "INTF") {
        return obj.getName().toLowerCase();
      }
    }
    */

    return undefined;
  }

  private findReadOrWriteReference(token: abaplint.Token) {
    const scope = this.findCurrentScopeByToken(token);
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

  private buildThisAttributes(def: abaplint.IClassDefinition, cName: string | undefined): string {
    let ret = "";
    for (const a of def.getAttributes()?.getAll() || []) {
      const escaped = Traversal.escapeNamespace(a.getName().toLowerCase());
      if (a.getMeta().includes(abaplint.IdentifierMeta.Static) === true) {
        ret += "this." + escaped + " = " + cName + "." + escaped + ";\n";
        continue;
      }
      const name = "this." + escaped;
      ret += name + " = " + new TranspileTypes().toType(a.getType()) + ";\n";
      ret += this.setValues(a, name);
    }
    return ret;
  }

  public buildConstructorContents(scope: abaplint.ISpaghettiScopeNode | undefined,
                                  def: abaplint.IClassDefinition): string {

    let ret = "";

    if (def.getSuperClass() !== undefined || def.getName().toUpperCase() === "CX_ROOT") {
      ret += "super();\n";
    }

    const cName = Traversal.escapeNamespace(def.getName().toLowerCase());

    ret += `this.me = new abap.types.ABAPObject();
this.me.set(this);
this.INTERNAL_ID = abap.internalIdCounter++;\n`;
    ret += this.buildThisAttributes(def, cName);

    // attributes from directly implemented interfaces(not interfaces implemented in super classes)
    for (const i of def.getImplementing()) {
      ret += this.dataFromInterfaces(i.name, scope, cName!);
      ret += this.aliasesFromInterfaces(i.name, scope, cName!);
    }

    // handle aliases after initialization of carrier variables
    for (const a of def.getAliases() || []) {
      ret += "this." + a.getName().toLowerCase() + " = this." + Traversal.escapeNamespace(a.getComponent().replace("~", "$").toLowerCase()) + ";\n";
    }
    // constants can be accessed both statically and via reference
    for (const c of def.getAttributes()?.getConstants() || []) {
      ret += "this." + Traversal.escapeNamespace(c.getName().toLowerCase()) + " = " + cName + "." + Traversal.escapeNamespace(c.getName().toLowerCase()) + ";\n";
    }

    return ret;
  }

  public findInterfaceDefinition(name: string, scope: abaplint.ISpaghettiScopeNode | undefined) {
    let intf = scope?.findInterfaceDefinition(name);
    if (intf === undefined) {
      const iglobal = this.reg.getObject("INTF", name) as abaplint.Objects.Interface | undefined;
      intf = iglobal?.getDefinition();
    }
    return intf;
  }

  public findTable(name: string) {
    const tabl = this.reg.getObject("TABL", name) as abaplint.Objects.Table | undefined;
    return tabl;
  }

  public findClassDefinition(name: string | undefined, scope: abaplint.ISpaghettiScopeNode | undefined) {
    if (name === undefined || scope === undefined) {
      return undefined;
    }
    let clas = scope.findClassDefinition(name);
    if (clas === undefined) {
      const iglobal = this.reg.getObject("CLAS", name) as abaplint.Objects.Class | undefined;
      clas = iglobal?.getDefinition();
    }
    return clas;
  }

  private dataFromInterfaces(name: string, scope: abaplint.ISpaghettiScopeNode | undefined, cname: string): string {
    let ret = "";

    const intf = this.findInterfaceDefinition(name, scope);

    for (const a of intf?.getAttributes().getConstants() || []) {
      const fname = Traversal.escapeNamespace(a.getName().toLowerCase());
      const iname = Traversal.escapeNamespace(intf?.getName().toLowerCase());
      if (intf?.isGlobal() === true) {
        ret += "this." + iname + "$" + fname + " = abap.Classes['" + intf?.getName().toUpperCase() + "']." + iname + "$" + fname + ";\n";
      } else {
        ret += "this." + iname + "$" + fname + " = " + iname + "." + iname + "$" + fname + ";\n";
      }
    }

    for (const a of intf?.getAttributes().getAll() || []) {
      const n = Traversal.escapeNamespace(name.toLowerCase()) + "$" + a.getName().toLowerCase();
      // note: interface inheritenace and super inheritance might be strange,
      if (a.getMeta().includes(abaplint.IdentifierMeta.Static) === true) {
        ret += "if (this." + n + " === undefined) this." + n + " = " + cname + "." + n + ";\n";
      } else {
        ret += "if (this." + n + " === undefined) this." + n + " = " + new TranspileTypes().toType(a.getType()) + ";\n";
      }
    }

    for (const i of intf?.getImplementing() || []) {
      ret += this.dataFromInterfaces(i.name, scope, cname);
    }

    return ret;
  }

  private aliasesFromInterfaces(name: string, scope: abaplint.ISpaghettiScopeNode | undefined, cname: string): string {
    let ret = "";

    const intf = this.findInterfaceDefinition(name, scope);

    for (const a of intf?.getAliases() || []) {
      const iname = Traversal.escapeNamespace(intf?.getName().toLowerCase());
      const aname = Traversal.escapeNamespace(a.getName().toLowerCase());
      const cname = Traversal.escapeNamespace(a.getComponent().toLowerCase().replace("~", "$"));
      ret += "this." + iname + "$" + aname + " = this." + cname + ";\n";
    }

    for (const i of intf?.getImplementing() || []) {
      ret += this.aliasesFromInterfaces(i.name, scope, cname);
    }

    return ret;
  }

  public determineType(node: abaplint.Nodes.ExpressionNode | abaplint.Nodes.StatementNode,
                       scope: abaplint.ISpaghettiScopeNode | undefined): abaplint.AbstractType | undefined {
    if (scope === undefined) {
      return undefined;
    }

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
      } else if (c.get() instanceof abaplint.Expressions.AttributeName
          && context instanceof abaplint.BasicTypes.ObjectReferenceType) {
        const id = context.getIdentifier();
        if (id instanceof abaplint.Types.ClassDefinition || id instanceof abaplint.Types.InterfaceDefinition) {
          const concat = c.concatTokens();
          if (concat.includes("~")) {
            const [iname, aname] = concat.split("~");
            const intf = this.findInterfaceDefinition(iname, scope);
            context = intf?.getAttributes().findByName(aname)?.getType();
          } else {
            context = id.getAttributes().findByName(concat)?.getType();
          }
        }
      } else if (c.get() instanceof abaplint.Expressions.AttributeName
          && context instanceof abaplint.BasicTypes.DataReference) {
        const type = context.getType();
        if (type instanceof abaplint.BasicTypes.StructureType) {
          context = type.getComponentByName(c.concatTokens());
        }
      }
    }

    return context;
  }

  public isInsideLoop(node: abaplint.Nodes.StatementNode): boolean {
    const stack: abaplint.Nodes.StatementNode[] = [];

    for (const statement of this.getFile().getStatements()) {
      const get = statement.get();
      if (get instanceof abaplint.Statements.Loop
          || get instanceof abaplint.Statements.While
          || get instanceof abaplint.Statements.SelectLoop
          || get instanceof abaplint.Statements.Do) {
        stack.push(statement);
      } else if (get instanceof abaplint.Statements.EndLoop
          || get instanceof abaplint.Statements.EndWhile
          || get instanceof abaplint.Statements.EndSelect
          || get instanceof abaplint.Statements.EndDo) {
        stack.pop();
      }
      if (statement === node) {
        break;
      }
    }

    return stack.length > 0;
  }

  public isInsideDoOrWhile(node: abaplint.Nodes.StatementNode): boolean {
    const stack: abaplint.Nodes.StatementNode[] = [];

    for (const statement of this.getFile().getStatements()) {
      const get = statement.get();
      if (get instanceof abaplint.Statements.While
          || get instanceof abaplint.Statements.Do) {
        stack.push(statement);
      } else if (get instanceof abaplint.Statements.EndWhile
          || get instanceof abaplint.Statements.EndDo) {
        stack.pop();
      }
      if (statement === node) {
        break;
      }
    }

    return stack.length > 0;
  }

  public registerClassOrInterface(def: abaplint.IClassDefinition | abaplint.IInterfaceDefinition | undefined): string {
    if (def === undefined) {
      return "";
    }
    const name = this.buildInternalName(def.getName(), def);
    return `abap.Classes['${name}'] = ${Traversal.escapeNamespace(def.getName().toLowerCase())};`;
  }

  public setValues(identifier: abaplint.TypedIdentifier, name: string) {
    return Traversal.setValues(identifier, name);
  }

  public static setValues(identifier: abaplint.TypedIdentifier, name: string) {
    const val = identifier.getValue();
    let ret = "";

    const handle = (val: any, name: string) => {
      if (typeof val === "string") {
        const e = ConstantTranspiler.escape(val);
        ret += name + ".set(" + e + ");\n";
      } else if (typeof val === "object") {
        const a: any = val;
        for (const v of Object.keys(val)) {
          const s = a[v];
          if (s === undefined) {
            continue;
          }
          handle(s, name + ".get()." + v.toLowerCase());
        }
      }
    };

    handle(val, Traversal.prefixVariable(name));

    return ret;
  }

  public buildInternalName(name: string, def: abaplint.IClassDefinition | abaplint.IInterfaceDefinition | undefined): string {
    if (def) {
      if (def.isGlobal() === false) {
        const prefix = this.buildPrefix();
        return `${prefix}${def?.getName()?.toUpperCase()}`;
      } else {
        return def?.getName()?.toUpperCase();
      }
    }

// assume global
    return name.toUpperCase();
  }

  public lookupClassOrInterface(name: string | undefined, token: abaplint.Token | undefined, directGlobal = false): string {
    if (name === undefined || token === undefined) {
      return "abap.Classes['undefined']";
    }

    if (directGlobal === true) {
      return "abap.Classes[" + name + ".trimEnd()]";
    }

    const scope = this.findCurrentScopeByToken(token);
    let def: abaplint.IClassDefinition | abaplint.IInterfaceDefinition | undefined = scope?.findClassDefinition(name);
    if (def === undefined) {
      def = scope?.findInterfaceDefinition(name);
    }

    const internalName = this.buildInternalName(name, def);
    return "abap.Classes['" + internalName + "']";
  }

  public buildPrefix(): string {
    return this.obj.getType() + "-" + this.obj.getName() + "-";
  }

////////////////////////////

  protected traverseStructure(node: abaplint.Nodes.StructureNode): Chunk {
    const list: any = StructureTranspilers;
    const ret = new Chunk();

    const search = node.get().constructor.name + "Transpiler";
    if (list[search]) {
      const transpiler = new list[search]() as IStructureTranspiler;
      ret.appendChunk(transpiler.transpile(node, this));
      return ret;
    }

    for (const c of node.getChildren()) {
      if (c instanceof abaplint.Nodes.StructureNode) {
        ret.appendChunk(this.traverseStructure(c));
      } else if (c instanceof abaplint.Nodes.StatementNode) {
        ret.appendChunk(this.traverseStatement(c));
      } else {
        throw new Error("traverseStructure, unexpected child node type");
      }
    }
    return ret;
  }

  protected traverseStatement(node: abaplint.Nodes.StatementNode): Chunk {
    const list: any = StatementTranspilers;
    const search = node.get().constructor.name + "Transpiler";
    if (list[search]) {
      const transpiler = new list[search]() as IStatementTranspiler;
      const chunk = transpiler.transpile(node, this);
      chunk.appendString("\n");
      return chunk;
    }
    throw new Error(`Statement ${node.get().constructor.name} not supported, ${node.concatTokens()}`);
  }

  protected traverseExpression(node: abaplint.Nodes.ExpressionNode): Chunk {
    const list: any = ExpressionTranspilers;
    const search = node.get().constructor.name + "Transpiler";
    if (list[search]) {
      const transpiler = new list[search]() as IExpressionTranspiler;
      return transpiler.transpile(node, this);
    }
    throw new Error(`Expression ${node.get().constructor.name} not supported, ${node.concatTokens()}`);
  }

}