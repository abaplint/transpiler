import * as abaplint from "@abaplint/core";
import {IStatementTranspiler} from "./_statement_transpiler";
import {Traversal} from "../traversal";
import {Chunk} from "../chunk";
import {EMLWalker, KERNEL_RAP_EML, abapFlag, abapString, emitCalls, unsupportedAddition} from "./_eml";

interface IOperation {
  kind: "CREATE" | "UPDATE" | "DELETE" | "EXECUTE";
  entity: string;
  action?: string;
  rows?: string;
  fields?: string[];
  setFields?: boolean;
  autoFillCid?: boolean;
}

/** MODIFY ENTITIES OF bdef ENTITY e CREATE|UPDATE|DELETE|EXECUTE ... MAPPED m FAILED f REPORTED r
 *
 * lowered to KERNEL_RAP_EML.begin with the MAPPED / FAILED / REPORTED
 * targets the statement names (the statement boundary: the kernel clears
 * them and binds scratch for the rest), then one modify / execute call per
 * operation in the order of the statement. Not lowered: the dynamic form (OPERATIONS), create by association (BY),
 * RELATING TO, AUGMENTING, and the deep forms. */
export class ModifyEntitiesTranspiler implements IStatementTranspiler {

  public transpile(node: abaplint.Nodes.StatementNode, traversal: Traversal): Chunk {
    const unsupported = unsupportedAddition(node, ["OPERATIONS", "RELATING", "AUGMENTING"]);
    if (unsupported !== undefined) {
      return new Chunk(`throw new Error("ModifyEntities ${unsupported}, not supported, transpiler");`);
    }

    const lookup = traversal.lookupClassOrInterface(KERNEL_RAP_EML, node.getFirstToken());
    const walker = new EMLWalker(node);
    const operations: IOperation[] = [];
    const targets: {[name: string]: string} = {};
    let bdef = "";
    let entity = "";
    let current: IOperation | undefined = undefined;

    walker.accept("MODIFY");
    if (walker.accept("ENTITIES", "OF")) {
      bdef = walker.takeName();
    } else if (walker.accept("ENTITY")) {
      // the short form names the root entity by its behavior definition
      walker.accept("IN", "LOCAL", "MODE");
      bdef = walker.takeName();
      entity = bdef;
    }

    while (!walker.done()) {
      const keyword = walker.takeKeyword();
      switch (keyword) {
        case "ENTITY":
          entity = walker.takeName();
          break;
        case "CREATE":
        case "UPDATE":
        case "DELETE":
          current = {kind: keyword, entity};
          operations.push(current);
          break;
        case "EXECUTE":
          current = {kind: "EXECUTE", entity, action: walker.takeName()};
          operations.push(current);
          break;
        case "FROM":
          if (current) {
            current.rows = traversal.traverse(walker.takeExpression()).getCode();
          }
          break;
        case "FIELDS":
          if (current) {
            current.fields = walker.takeFieldList();
          }
          break;
        case "SET":
          // SET FIELDS WITH
          walker.accept("FIELDS");
          if (current) {
            current.setFields = true;
          }
          break;
        case "WITH":
          if (current) {
            current.rows = traversal.traverse(walker.takeExpression()).getCode();
          }
          break;
        case "AUTO":
          walker.accept("FILL", "CID");
          if (current) {
            current.autoFillCid = true;
          }
          break;
        case "MAPPED":
        case "FAILED":
        case "REPORTED":
        case "RESULT":
          targets[keyword.toLowerCase()] = traversal.traverse(walker.takeExpression()).getCode();
          break;
        default:
          // IN LOCAL MODE, PRIVILEGED, FORWARDING, the final period
          break;
      }
    }

    const calls: string[] = [];
    const statement: string[] = [`bdef: ${abapString(bdef)}`];
    for (const name of ["mapped", "failed", "reported"]) {
      if (targets[name] !== undefined) {
        statement.push(`${name}: ${targets[name]}`);
      }
    }
    calls.push(`await ${lookup}.begin({${statement.join(", ")}});`);
    for (const op of operations) {
      const options: string[] = [`bdef: ${abapString(bdef)}`, `entity: ${abapString(op.entity)}`];
      if (op.kind === "EXECUTE") {
        options.push(`action: ${abapString(op.action ?? "")}`);
      } else {
        options.push(`operation: ${abapString(op.kind)}`);
      }
      options.push(`rows: ${op.rows ?? "undefined"}`);
      if (op.fields !== undefined) {
        options.push(`fields: ${abapString(op.fields.join(","))}`);
      }
      if (op.setFields === true) {
        options.push(`set_fields: ${abapFlag()}`);
      }
      if (op.autoFillCid === true) {
        options.push(`auto_fill_cid: ${abapFlag()}`);
      }
      if (op.kind === "EXECUTE" && targets["result"] !== undefined) {
        options.push(`result: ${targets["result"]}`);
      }
      const method = op.kind === "EXECUTE" ? "execute" : "modify";
      calls.push(`await ${lookup}.${method}({${options.join(", ")}});`);
    }

    return emitCalls("ModifyEntities", lookup, calls, node, traversal);
  }

}
