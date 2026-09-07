import * as abaplint from "@abaplint/core";

const cds = abaplint.ExpressionsCDS;
type Node = abaplint.Nodes.ExpressionNode | abaplint.Nodes.TokenNode;

/** SQL for the supported relational subset of CDS. Never discard unknown syntax. */
export class CDSDatabaseView {
  private viewName = "";
  private readonly sources: string[] = [];

  public constructor(private readonly reg: abaplint.IRegistry) {}

  public build(obj: abaplint.Objects.DataDefinition): string | undefined {
    this.viewName = obj.getName();
    // Parse with the plugin's core so expression class identity also works when
    // the registry came from a webpack bundle or a different installation.
    const tree = new abaplint.CDSParser().parse(obj.findSourceFile());
    if (tree === undefined) {
      return this.unsupported("could not parse CDS definition");
    } else if (!(tree.get() instanceof cds.CDSDefineView)) {
      return undefined; // Abstract/custom entities have no database view.
    }

    const name = tree.findDirectExpression(cds.CDSName);
    const select = tree.findDirectExpression(cds.CDSSelect);
    if (name === undefined || select === undefined) {
      return this.unsupported("expected a SELECT view");
    }
    this.viewName = this.name(name);
    for (const child of tree.getChildren()) {
      if (child === name || child === select || child.get() instanceof cds.CDSAnnotation) {
        continue;
      }
      this.keyword(child, ["DEFINE", "ROOT", "VIEW", "ENTITY", "AS", ";"]);
    }

    const source = select.findDirectExpression(cds.CDSSource);
    if (source === undefined) {
      return this.unsupported("missing source");
    }
    const from = [this.source(source)];
    for (const join of select.findDirectExpressions(cds.CDSJoin)) {
      from.push(this.join(join));
    }
    const elements = select.findDirectExpressions(cds.CDSElement);
    if (elements.length === 0) {
      return this.unsupported("expected explicit projected fields");
    }
    const columns = elements.map(e => this.element(e)).join(", ");
    let where = "";
    for (const child of select.getChildren()) {
      if (child === source || child.get() instanceof cds.CDSJoin || child.get() instanceof cds.CDSElement) {
        continue;
      } else if (child.get() instanceof cds.CDSWhere) {
        where = " " + this.expression(child);
      } else {
        this.keyword(child, ["SELECT", "DISTINCT", "FROM", "{", "}", ","]);
      }
    }
    const distinct = select.findDirectTokenByText("DISTINCT") ? "DISTINCT " : "";
    return `CREATE VIEW ${this.quote(this.viewName)} AS SELECT ${distinct}${columns} FROM ${from.join(" ")}${where};`;
  }

  private source(node: abaplint.Nodes.ExpressionNode): string {
    const nameNode = node.findDirectExpression(cds.CDSPrefixedName);
    if (nameNode === undefined) {
      return this.unsupported("expected a table source");
    }
    const name = this.name(nameNode);
    if (this.reg.getObject("TABL", name)?.getType() !== "TABL") {
      return this.unsupported(`source ${name} does not resolve to a TABL`);
    }
    const as = node.findDirectExpression(cds.CDSAs);
    const alias = as?.findDirectExpression(cds.CDSName) || node.findDirectExpression(cds.CDSName);
    for (const child of node.getChildren()) {
      if (child !== nameNode && child !== as && child !== alias) {
        return this.unsupported("source parameters, filters or nested joins");
      }
    }
    this.sources.push(alias ? this.name(alias) : name);
    return this.quote(name) + (alias ? " AS " + this.quote(this.name(alias)) : "");
  }

  private join(node: abaplint.Nodes.ExpressionNode): string {
    const sources = node.findDirectExpressions(cds.CDSSource);
    const conditions = node.findDirectExpressions(cds.CDSCondition);
    const words = node.getDirectTokens().map(t => t.getStr().toUpperCase()).join(" ");
    const match = /^(?:(INNER|LEFT OUTER|RIGHT OUTER|CROSS) )?JOIN(?: ON)?$/.exec(words);
    if (sources.length !== 1 || match === null) {
      return this.unsupported("join shape: " + node.concatTokens());
    }
    const kind = match[1] || "INNER";
    if (conditions.length !== (kind === "CROSS" ? 0 : 1)) {
      return this.unsupported("join requires an ON condition (except CROSS JOIN)");
    }
    const source = this.source(sources[0]);
    return kind + " JOIN " + source + (conditions.length ? " ON " + this.expression(conditions[0]) : "");
  }

  private element(node: abaplint.Nodes.ExpressionNode): string {
    const field = node.findDirectExpression(cds.CDSPrefixedName);
    const as = node.findDirectExpression(cds.CDSAs);
    for (const child of node.getChildren()) {
      if (child === field || child === as || child.get() instanceof cds.CDSAnnotation) {
        continue;
      }
      this.keyword(child, ["KEY"]);
    }
    if (field === undefined) {
      return this.unsupported("expected a projected source field");
    }
    const parts = this.fieldParts(field);
    const alias = as?.findDirectExpression(cds.CDSName);
    return this.field(field) + " AS " + this.quote(alias ? this.name(alias) : parts[parts.length - 1]);
  }

  private expression(node: Node): string {
    if (node instanceof abaplint.Nodes.TokenNode) {
      return this.keyword(node, ["WHERE", "AND", "OR", "NOT", "IS", "NULL", "LIKE", "ESCAPE", "BETWEEN",
        "=", "!", "<", ">", "(", ")"]);
    } else if (node.get() instanceof cds.CDSPrefixedName) {
      if (node.getTokens().map(t => t.getStr()).join("").toLowerCase() === "$session.system_language") {
        return "'E'";
      }
      return this.field(node);
    } else if (node.get() instanceof cds.CDSString || node.get() instanceof cds.CDSInteger) {
      return node.getTokens().map(t => t.getStr()).join("");
    } else if (node.get() instanceof cds.CDSCondition || node.get() instanceof cds.CDSWhere) {
      const parts: string[] = [];
      for (const child of node.getChildren()) {
        const part = this.expression(child);
        if (["<", ">", "!"].includes(parts[parts.length - 1]) && ["=", ">"].includes(part)) {
          parts[parts.length - 1] += part;
        } else {
          parts.push(part);
        }
      }
      return parts.join(" ");
    }
    return this.unsupported(node.concatTokens());
  }

  private field(node: abaplint.Nodes.ExpressionNode): string {
    const parts = this.fieldParts(node);
    if (parts.length === 1 && this.sources.length === 1) {
      parts.unshift(this.sources[0]);
    } else if (parts.length === 2 && !this.sources.includes(parts[0])) {
      return this.unsupported("unknown source qualifier " + parts[0]);
    }
    return parts.map(p => this.quote(p)).join(".");
  }

  private fieldParts(node: abaplint.Nodes.ExpressionNode): string[] {
    const text = node.getTokens().map(t => t.getStr()).join("").toLowerCase();
    const parts = text.split(".");
    if (parts.length > 2 || parts.some(p => !/^(\/\w+\/)?\w+$/.test(p))) {
      return this.unsupported("field path " + text);
    }
    return parts;
  }

  private name(node: abaplint.Nodes.ExpressionNode): string {
    const name = node.getTokens().map(t => t.getStr()).join("").toLowerCase();
    if (!/^(\/\w+\/)?\w+$/.test(name)) {
      return this.unsupported("identifier " + name);
    }
    return name;
  }

  private keyword(node: Node, allowed: string[]): string {
    const text = node.concatTokens().toUpperCase();
    if (!(node instanceof abaplint.Nodes.TokenNode) || !allowed.includes(text)) {
      return this.unsupported(node.concatTokens());
    }
    return text;
  }

  private quote(name: string): string {
    return '"' + name.toLowerCase().replace(/"/g, '""') + '"';
  }

  private unsupported(detail: string): never {
    throw new Error(`CDS view ${this.viewName}: unsupported database definition: ${detail}`);
  }
}
