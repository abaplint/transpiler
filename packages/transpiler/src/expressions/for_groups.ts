import {BasicTypes, Expressions, Nodes} from "@abaplint/core";
import {Traversal} from "../traversal";
import {FieldSymbolTranspiler} from "../statements";
import {SourceFieldSymbolTranspiler} from "./source_field_symbol";
import {UniqueIdentifier} from "../unique_identifier";

export interface ForGroupsDescriptor {
  /** declarations to emit before the loop, eg. inline field symbols */
  declarations: string[];
  open: string;
  preBody: string[];
  close: string;
}

/** FOR GROUPS group OF member IN itab GROUP BY key [ASCENDING|DESCENDING] [WITHOUT MEMBERS] */
export class ForGroupsTranspiler {

  public transpile(forExpression: Nodes.ExpressionNode, traversal: Traversal): ForGroupsDescriptor {
    const targets = forExpression.getChildren().filter(c => c instanceof Nodes.ExpressionNode
      && (c.get() instanceof Expressions.TargetField || c.get() instanceof Expressions.TargetFieldSymbol)) as Nodes.ExpressionNode[];
    const source = forExpression.findDirectExpression(Expressions.Source);
    if (targets.length !== 2 || source === undefined) {
      throw new Error("FOR GROUPS, unexpected structure, " + forExpression.concatTokens());
    }
    if (forExpression.findDirectExpression(Expressions.LoopGroupByComponent)) {
      throw new Error("FOR GROUPS, GROUP BY with components, todo, " + forExpression.concatTokens());
    }
    const groupBy = forExpression.findDirectExpression(Expressions.FieldChain);
    if (groupBy === undefined) {
      throw new Error("FOR GROUPS, GROUP BY expected, " + forExpression.concatTokens());
    }
    if (forExpression.findDirectTokenByText("TEXT")) {
      throw new Error("FOR GROUPS, AS TEXT, todo, " + forExpression.concatTokens());
    }

    const declarations: string[] = [];
    const [groupNode, memberNode] = targets;
    const row = UniqueIdentifier.get();
    const loopEntry = UniqueIdentifier.get();

    let memberInGenerator: string;
    let memberInBody: string;
    if (memberNode.get() instanceof Expressions.TargetFieldSymbol) {
      declarations.push(this.declareFieldSymbol(memberNode, traversal));
      const name = new SourceFieldSymbolTranspiler().transpile(memberNode, traversal).getCode();
      memberInGenerator = `${name}.assign(${row});`;
      memberInBody = `${name}.assign(${loopEntry}.members[0]);`;
    } else {
      const name = traversal.traverse(memberNode).getCode();
      memberInGenerator = `const ${name} = ${row}.clone();`;
      memberInBody = `const ${name} = ${loopEntry}.members[0].clone();`;
    }

    let groupInBody: string;
    if (groupNode.get() instanceof Expressions.TargetFieldSymbol) {
      declarations.push(this.declareFieldSymbol(groupNode, traversal));
      const name = new SourceFieldSymbolTranspiler().transpile(groupNode, traversal).getCode();
      groupInBody = `${name}.assign(${loopEntry}.key);`;
    } else {
      groupInBody = `const ${traversal.traverse(groupNode).getCode()} = ${loopEntry}.key.clone();`;
    }

    const groups = UniqueIdentifier.get();
    const key = UniqueIdentifier.get();
    const rawKey = UniqueIdentifier.get();
    const entry = UniqueIdentifier.get();

    let ordered = `${groups}.values()`;
    if (forExpression.findDirectTokenByText("ASCENDING")) {
      ordered = `[...${groups}.values()].sort((a, b) => abap.compare.lt(a.key, b.key) ? -1 : abap.compare.gt(a.key, b.key) ? 1 : 0)`;
    } else if (forExpression.findDirectTokenByText("DESCENDING")) {
      ordered = `[...${groups}.values()].sort((a, b) => abap.compare.gt(a.key, b.key) ? -1 : abap.compare.lt(a.key, b.key) ? 1 : 0)`;
    }

    // groups are kept in order of first occurrence
    const generator = `(async function*() {\n`
      + `const ${groups} = new Map();\n`
      + `for await (const ${row} of abap.statements.loop(${traversal.traverse(source).getCode()})) {\n`
      + `${memberInGenerator}\n`
      + `const ${key} = ${traversal.traverse(groupBy).getCode()};\n`
      + `const ${rawKey} = ${key}.get();\n`
      + `let ${entry} = ${groups}.get(${rawKey});\n`
      + `if (${entry} === undefined) {\n`
      + `${entry} = {key: ${key}.clone(), members: []};\n`
      + `${groups}.set(${rawKey}, ${entry});\n`
      + `}\n`
      + `${entry}.members.push(${row});\n`
      + `}\n`
      + `for (const value of ${ordered}) {\n`
      + `yield value;\n`
      + `}\n`
      + `})()`;

    return {
      declarations,
      open: `for await (const ${loopEntry} of ${generator}) {`,
      preBody: [groupInBody, memberInBody],
      close: "}",
    };
  }

  /** abaplint does not always add the FOR GROUPS targets to the scope, or types them void,
   *  then the field symbol is declared untyped, it is only ever assigned to rows of the source */
  private declareFieldSymbol(node: Nodes.ExpressionNode, traversal: Traversal): string {
    const token = node.getFirstToken();
    const found = traversal.findCurrentScopeByToken(token)?.findVariable(token.getStr().replace(/^!/, ""));
    if (found !== undefined && !(found.getType() instanceof BasicTypes.VoidType)) {
      return new FieldSymbolTranspiler().transpile(node, traversal).getCode();
    }
    const name = new SourceFieldSymbolTranspiler().transpile(node, traversal).getCode();
    return `let ${name} = new abap.types.FieldSymbol();`;
  }

}
