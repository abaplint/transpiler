import * as abaplint from "@abaplint/core";
import {IStatementTranspiler} from "./_statement_transpiler";
import {Traversal} from "../traversal";
import {Chunk} from "../chunk";
import {EMLWalker, KERNEL_RAP_EML, abapString, emitCalls, unsupportedAddition} from "./_eml";

interface IRead {
  entity: string;
  keys?: string;
  /** the FIELDS ( ) list, "*" for ALL FIELDS, undefined for FROM */
  fields?: string;
  result?: string;
}

/** READ ENTITIES OF bdef ENTITY e FROM|ALL FIELDS WITH|FIELDS ( ) WITH keys RESULT r FAILED f REPORTED r
 *
 * lowered to KERNEL_RAP_EML.begin with the FAILED / REPORTED targets the
 * statement names (the statement boundary), then one read call per read in
 * the order of the statement. Not lowered: read by association (BY), LINK, and EXECUTE
 * (functions). */
export class ReadEntitiesTranspiler implements IStatementTranspiler {

  public transpile(node: abaplint.Nodes.StatementNode, traversal: Traversal): Chunk {
    const unsupported = unsupportedAddition(node, ["LINK", "EXECUTE"]);
    if (unsupported !== undefined) {
      return new Chunk(`throw new Error("ReadEntities ${unsupported}, not supported, transpiler");`);
    }

    const lookup = traversal.lookupClassOrInterface(KERNEL_RAP_EML, node.getFirstToken());
    const walker = new EMLWalker(node);
    const reads: IRead[] = [];
    const targets: {[name: string]: string} = {};
    let bdef = "";
    let entity = "";
    let current: IRead | undefined = undefined;

    walker.accept("READ");
    if (walker.accept("ENTITIES", "OF")) {
      bdef = walker.takeName();
    } else if (walker.accept("ENTITY")) {
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
        case "FROM":
          current = {entity, keys: traversal.traverse(walker.takeExpression()).getCode()};
          reads.push(current);
          break;
        case "ALL":
          walker.accept("FIELDS", "WITH");
          current = {entity, fields: "*", keys: traversal.traverse(walker.takeExpression()).getCode()};
          reads.push(current);
          break;
        case "FIELDS":
          current = {entity, fields: walker.takeFieldList().join(",")};
          walker.accept("WITH");
          current.keys = traversal.traverse(walker.takeExpression()).getCode();
          reads.push(current);
          break;
        case "RESULT":
          if (current) {
            current.result = traversal.traverse(walker.takeExpression()).getCode();
          }
          break;
        case "FAILED":
        case "REPORTED":
          targets[keyword.toLowerCase()] = traversal.traverse(walker.takeExpression()).getCode();
          break;
        default:
          // IN LOCAL MODE, the final period
          break;
      }
    }

    const calls: string[] = [];
    const statement: string[] = [`bdef: ${abapString(bdef)}`];
    for (const name of ["failed", "reported"]) {
      if (targets[name] !== undefined) {
        statement.push(`${name}: ${targets[name]}`);
      }
    }
    calls.push(`await ${lookup}.begin({${statement.join(", ")}});`);
    for (const read of reads) {
      const options: string[] = [`bdef: ${abapString(bdef)}`, `entity: ${abapString(read.entity)}`, `keys: ${read.keys ?? "undefined"}`];
      if (read.fields !== undefined) {
        options.push(`fields: ${abapString(read.fields)}`);
      }
      if (read.result !== undefined) {
        options.push(`result: ${read.result}`);
      }
      calls.push(`await ${lookup}.read({${options.join(", ")}});`);
    }

    return emitCalls("ReadEntities", lookup, calls, node, traversal);
  }

}
