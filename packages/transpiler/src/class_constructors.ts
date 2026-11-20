import * as abaplint from "@abaplint/core";
import {Traversal} from "./traversal";

/** Class constructors run at the first use of their class, as on a system, not when the code is loaded:
 *  - static method call, NEW, CREATE OBJECT: at the statement, the static methods check their own class
 *  - static attribute: on entry of the procedure containing the access, also in a branch that is not taken
 *  Only classes with a class constructor in their chain get checks, see abap.classConstructor in the runtime */
export class ClassConstructors {
  private static readonly kernel = new WeakMap<abaplint.IClassDefinition, boolean>();

  /** true when the class or one of its super classes has a class constructor, not counting kernel ones */
  public static inChain(def: abaplint.IClassDefinition | undefined, traversal: Traversal,
                        scope: abaplint.ISpaghettiScopeNode | undefined): boolean {
    let current = def;
    for (let guard = 0; current !== undefined && guard < 100; guard++) {
      if (current.getMethodDefinitions()?.getByName("CLASS_CONSTRUCTOR") !== undefined
          && ClassConstructors.isKernel(current, traversal) === false) {
        return true;
      }
      const superName = current.getSuperClass();
      if (superName === undefined) {
        return false;
      }
      current = traversal.findClassDefinition(superName, scope);
    }
    return false;
  }

  /** the statement that runs the class constructors of clas, the JS code of a class, if not done yet */
  public static check(clas: string): string {
    return `if (${clas}.INTERNAL_CCTOR_DONE === false) await abap.classConstructor(${clas});\n`;
  }

  /** the same as an expression, true */
  public static checkExpression(clas: string): string {
    return `(${clas}.INTERNAL_CCTOR_DONE === false && await abap.classConstructor(${clas}), true)`;
  }

  /** checks for the declaring classes of the static attributes accessed as class=>attr in node,
   *  constructed: names of classes whose class constructors have run for sure, eg. in a method of the class */
  public static entryChecks(node: abaplint.INode, traversal: Traversal, constructed?: Set<string>): string {
    const found = new Map<string, string>();
    ClassConstructors.collect(node, traversal, found, constructed);
    let ret = "";
    for (const clas of found.values()) {
      ret += ClassConstructors.check(clas);
    }
    return ret;
  }

  /** the upper case names of the class and its super classes: while a method of the class runs,
   *  their class constructors have run */
  public static chainNames(def: abaplint.IClassDefinition | undefined, traversal: Traversal,
                           scope: abaplint.ISpaghettiScopeNode | undefined): Set<string> {
    const ret = new Set<string>();
    let current = def;
    for (let guard = 0; current !== undefined && guard < 100; guard++) {
      ret.add(current.getName().toUpperCase());
      const superName = current.getSuperClass();
      current = superName === undefined ? undefined : traversal.findClassDefinition(superName, scope);
    }
    return ret;
  }

  private static collect(node: abaplint.INode, traversal: Traversal, found: Map<string, string>,
                         constructed: Set<string> | undefined): void {
    const children = node.getChildren();
    for (let i = 0; i < children.length; i++) {
      const child = children[i];
      const arrow = children[i + 1];
      const attribute = children[i + 2];
      if (child instanceof abaplint.Nodes.ExpressionNode
          && child.get() instanceof abaplint.Expressions.ClassName
          && arrow instanceof abaplint.Nodes.TokenNode && arrow.getFirstToken().getStr() === "=>"
          && attribute instanceof abaplint.Nodes.ExpressionNode && attribute.get() instanceof abaplint.Expressions.AttributeName) {
        ClassConstructors.add(child, attribute.concatTokens(), traversal, found, constructed);
      }
      if (child instanceof abaplint.Nodes.StructureNode && ClassConstructors.isOwnProcedure(child)) {
        // the code of other procedures and classes runs when they are entered, not here
        continue;
      } else if (!(child instanceof abaplint.Nodes.TokenNode)) {
        ClassConstructors.collect(child, traversal, found, constructed);
      }
    }
  }

  /** a class constructor with @KERNEL code is part of the runtime, eg. cl_abap_char_utilities setting the values of
   *  constants a system has built in. It runs when the code is loaded, as other code reads those values at load */
  public static isKernel(def: abaplint.IClassDefinition, traversal: Traversal): boolean {
    let ret = ClassConstructors.kernel.get(def);
    if (ret === undefined) {
      ret = false;
      const file = traversal.reg.getFileByName(def.getFilename());
      const obj = file === undefined ? undefined : traversal.reg.findObjectForFile(file);
      for (const abapFile of obj instanceof abaplint.ABAPObject ? obj.getABAPFiles() : []) {
        for (const impl of abapFile.getStructure()?.findAllStructures(abaplint.Structures.ClassImplementation) || []) {
          const name = impl.findFirstStatement(abaplint.Statements.ClassImplementation)
            ?.findFirstExpression(abaplint.Expressions.ClassName)?.concatTokens().toUpperCase();
          if (name !== def.getName().toUpperCase()) {
            continue;
          }
          for (const method of impl.findAllStructures(abaplint.Structures.Method)) {
            const methodName = method.findFirstStatement(abaplint.Statements.MethodImplementation)
              ?.findFirstExpression(abaplint.Expressions.MethodName)?.concatTokens().toUpperCase();
            if (methodName === "CLASS_CONSTRUCTOR" && method.concatTokens().includes("@KERNEL")) {
              ret = true;
            }
          }
        }
      }
      ClassConstructors.kernel.set(def, ret);
    }
    return ret;
  }

  private static isOwnProcedure(node: abaplint.Nodes.StructureNode): boolean {
    const get = node.get();
    return get instanceof abaplint.Structures.ClassImplementation
      || get instanceof abaplint.Structures.ClassDefinition
      || get instanceof abaplint.Structures.Interface
      || get instanceof abaplint.Structures.Form
      || get instanceof abaplint.Structures.FunctionModule
      || get instanceof abaplint.Structures.Method
      || get instanceof abaplint.Structures.Module;
  }

  private static add(className: abaplint.Nodes.ExpressionNode, attribute: string, traversal: Traversal,
                     found: Map<string, string>, constructed: Set<string> | undefined): void {
    const token = className.getFirstToken();
    const scope = traversal.findCurrentScopeByToken(token);
    const name = attribute.toUpperCase();
    let def = traversal.findClassDefinition(className.concatTokens(), scope);
    // the class declaring the attribute, constants do not run a class constructor
    for (let guard = 0; def !== undefined && guard < 100; guard++) {
      const attributes = def.getAttributes();
      if (attributes === undefined) {
        return;
      } else if (attributes.getConstants().some(c => c.getName().toUpperCase() === name)) {
        return;
      }
      if (attributes.getStatic().some(a => a.getName().toUpperCase() === name)) {
        break;
      }
      const superName = def.getSuperClass();
      def = superName === undefined ? undefined : traversal.findClassDefinition(superName, scope);
    }
    if (def === undefined || ClassConstructors.inChain(def, traversal, scope) === false) {
      return;
    }
    const key = def.getName().toUpperCase();
    if (found.has(key) === false && constructed?.has(key) !== true) {
      found.set(key, ClassConstructors.jsName(def, token, traversal));
    }
  }

  /** a local class of the object being transpiled is a class in the same module, used directly,
   *  others via abap.Classes */
  private static jsName(def: abaplint.IClassDefinition, token: abaplint.Token, traversal: Traversal): string {
    const obj = traversal.getCurrentObject();
    if (def.isGlobal() === false && obj.getABAPFiles().some(f => f.getFilename() === def.getFilename())) {
      return Traversal.escapeNamespace(def.getName().toLowerCase())!;
    }
    return traversal.lookupClassOrInterface(def.getName(), token);
  }

}
