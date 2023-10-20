import * as abaplint from "@abaplint/core";
import {IStatementTranspiler} from "./_statement_transpiler";
import {TranspileTypes} from "../transpile_types";
import {Traversal} from "../traversal";
import {ConstantTranspiler, FieldChainTranspiler} from "../expressions";
import {Chunk} from "../chunk";
import {UniqueIdentifier} from "../unique_identifier";

export class MethodImplementationTranspiler implements IStatementTranspiler {

  public transpile(node: abaplint.Nodes.StatementNode, traversal: Traversal): Chunk {
    const token = node.findFirstExpression(abaplint.Expressions.MethodName)!.getFirstToken();
    let methodName = token.getStr();

    const scope = traversal.findCurrentScopeByToken(token);
    if (scope === undefined) {
      throw new Error("MethodTranspiler, scope not found, " + methodName + ", " + traversal.getFilename());
    } else if (scope.getIdentifier().sname !== methodName) {
      throw new Error("MethodTranspiler, wrong scope found, " + scope.getIdentifier().sname + ", " + methodName);
    }

    let after = "";

    const classDef = traversal.getClassDefinition(token);

    let unique = "";
    if (methodName.toUpperCase() === "CONSTRUCTOR" && classDef) {
// note that all ABAP identifiers are lower cased, sometimes the kernel does magic, so it needs to know the method input name
      unique = "INPUT";
//      after = traversal.buildConstructorContents(scope.getParent(), classDef);
      methodName = "constructor_";
    }

    const methodDef = this.findMethodParameters(scope);

    const vars = scope.getData().vars;
    for (const n in vars) {
      const identifier = vars[n];
      const varName = n.toLowerCase();
      if (identifier.getMeta().includes(abaplint.IdentifierMeta.MethodImporting)
          || identifier.getMeta().includes(abaplint.IdentifierMeta.MethodChanging)
          || identifier.getMeta().includes(abaplint.IdentifierMeta.EventParameter)
          || identifier.getMeta().includes(abaplint.IdentifierMeta.MethodExporting)) {
        if (unique === "") {
          unique = "INPUT";
        }

        const parameterDefault = methodDef?.getParameterDefault(varName);
//        const isOptional = methodDef?.getOptional().includes(varName.toUpperCase());
        const passByValue = identifier.getMeta().includes(abaplint.IdentifierMeta.PassByValue);

        const type = identifier.getType();
        if (identifier.getMeta().includes(abaplint.IdentifierMeta.MethodExporting)) {
          after += `let ${varName} = ${unique}?.${varName} || ${new TranspileTypes().toType(identifier.getType())};\n`;
        } else if (identifier.getMeta().includes(abaplint.IdentifierMeta.MethodImporting) && type.isGeneric() === false) {
          after += new TranspileTypes().declare(identifier) + "\n";
          // note: it might be nessesary to do a type conversion, eg char is passed to xstring parameter
          after += "if (" + unique + " && " + unique + "." + varName + ") {" + varName + ".set(" + unique + "." + varName + ");}\n";
        } else {
          after += new TranspileTypes().declare(identifier) + "\n";
          after += "if (" + unique + " && " + unique + "." + varName + ") {" + varName + " = " + unique + "." + varName + ";}\n";
        }

        if (parameterDefault) {
          let val = "";
          if (parameterDefault.get() instanceof abaplint.Expressions.Constant) {
            val = new ConstantTranspiler().transpile(parameterDefault, traversal).getCode();
          } else if (parameterDefault.get() instanceof abaplint.Expressions.FieldChain) {
            if (parameterDefault.getFirstToken().getStr().toLowerCase() === "abap_true") {
              val = "abap.builtin.abap_true";
            } else if (parameterDefault.getFirstToken().getStr().toLowerCase() === "abap_false") {
              val = "abap.builtin.abap_false";
            } else if (parameterDefault.getFirstToken().getStr().toLowerCase() === "abap_undefined") {
              val = "abap.builtin.abap_undefined";
            } else if (parameterDefault.getFirstToken().getStr().toLowerCase() === "space") {
              val = "abap.builtin.space";
            } else if (parameterDefault.concatTokens().toLowerCase() === "sy-langu") {
              val = "abap.builtin.sy.get().langu";
            } else if (parameterDefault.concatTokens().toLowerCase() === "sy-mandt") {
              val = "abap.builtin.sy.get().mandt";
            } else if (parameterDefault.concatTokens().toLowerCase() === "sy-uname") {
              val = "abap.builtin.sy.get().uname";
            } else if (parameterDefault.concatTokens().toLowerCase() === "sy-sysid") {
              val = "abap.builtin.sy.get().sysid";
            } else if (parameterDefault.concatTokens().toLowerCase() === "sy-msgid") {
              val = "abap.builtin.sy.get().msgid";
            } else if (parameterDefault.concatTokens().toLowerCase() === "sy-msgty") {
              val = "abap.builtin.sy.get().msgty";
            } else if (parameterDefault.concatTokens().toLowerCase() === "sy-msgno") {
              val = "abap.builtin.sy.get().msgno";
            } else if (parameterDefault.concatTokens().toLowerCase() === "sy-msgv1") {
              val = "abap.builtin.sy.get().msgv1";
            } else if (parameterDefault.concatTokens().toLowerCase() === "sy-msgv2") {
              val = "abap.builtin.sy.get().msgv2";
            } else if (parameterDefault.concatTokens().toLowerCase() === "sy-msgv3") {
              val = "abap.builtin.sy.get().msgv3";
            } else if (parameterDefault.concatTokens().toLowerCase() === "sy-msgv4") {
              val = "abap.builtin.sy.get().msgv4";
            } else {
              // note: this can be difficult, the "def" might be from an interface, ie. a different scope than the method
              val = new FieldChainTranspiler().transpile(parameterDefault, traversal, true, methodDef?.getFilename(), true).getCode();
              if (val.startsWith(parameterDefault.getFirstToken().getStr().toLowerCase()) === true) {
                val = "this." + val;
              }
            }
          } else {
            throw new Error("MethodImplementationTranspiler, unknown default param type");
          }
          if (passByValue === true || identifier.getMeta().includes(abaplint.IdentifierMeta.MethodChanging)) {
            after += "if (" + unique + " === undefined || " + unique + "." + varName + " === undefined) {" + varName + ".set(" + val + ");}\n";
          } else {
            after += "if (" + unique + " === undefined || " + unique + "." + varName + " === undefined) {" + varName + " = " + val + ";}\n";
          }
        }
      } else if (identifier.getMeta().includes(abaplint.IdentifierMeta.MethodReturning)) {
        after = after + new TranspileTypes().declare(identifier) + "\n";
      }
    }

    if (after.length > 0) { // argh
      after = "\n" + after;
      after = after.substring(0, after.length - 1);
    }

    const method = this.findMethod(methodName, classDef, traversal);
    let staticMethod = "";

    methodName = methodName.replace("~", "$").toLowerCase();
    if (methodName === "then") {
// todo, this should be a checked in the validation step, but no abaplint rule for it?
// it messes up promises when "this" is returned
      throw new Error(`Method name "then" not allowed`);
    }

    const superDef = traversal.findClassDefinition(classDef?.getSuperClass(), scope);
    for (const a of superDef?.getAliases().getAll() || []) {
      if (a.getName().toLowerCase() === methodName) {
        methodName = a.getComponent().replace("~", "$").toLowerCase();
      }
    }

    if (method && method.isStatic()) {
      // in ABAP static methods can be called with instance arrows, "->"
      const className = scope.getParent()?.getIdentifier().sname?.toLowerCase();
      staticMethod = "async " + Traversal.escapeNamespace(methodName) + "(" + unique + ") {\n" +
        "return " + Traversal.escapeNamespace(className) + "." + Traversal.escapeNamespace(methodName) + "(" + unique + ");\n" +
        "}\n" + "static ";
    }

    UniqueIdentifier.resetIndexBackup();

    const str = staticMethod + "async " + Traversal.escapeNamespace(methodName) + "(" + unique + ") {" + after;
    return new Chunk().append(str, node, traversal);
  }

/////////////////////////////

  private findMethod(name: string, cdef: abaplint.IClassDefinition | undefined, traversal: Traversal) {
    if (cdef === undefined) {
      return undefined;
    }

    if (name.includes("~")) {
      const split = name.split("~");
      const intfName = split[0];
      name = split[1];
      const scope = traversal.findCurrentScopeByToken(cdef.getToken());
      const intf = traversal.findInterfaceDefinition(intfName, scope);
      return intf?.getMethodDefinitions()?.getByName(name);
    } else {
      return cdef.getMethodDefinitions()?.getByName(name);
    }
  }

  private findMethodParameters(scope: abaplint.ISpaghettiScopeNode): abaplint.Types.MethodParameters | undefined {
    for (const r of scope.getData().references) {
      if (r.referenceType === abaplint.ReferenceType.MethodImplementationReference
          && r.resolved instanceof abaplint.Types.MethodDefinition) {
        return r.resolved.getParameters();
      }
    }
    return undefined;
  }

}