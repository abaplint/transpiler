import * as abaplint from "@abaplint/core";

type Join = {LTAB: string, LFIELD: string, OPERATOR: string, RTAB: string, RFIELD?: string};

const OPERATORS: {[operator: string]: string} = {EQ: "=", NE: "<>", LT: "<", LE: "<=", GT: ">", GE: ">="};

export class DatabaseSchemaReuse {
  private readonly myQuote: string;

  public constructor(quote: string) {
    this.myQuote = quote;
  }

  private quote(name: string): string {
    return this.myQuote + name.toLowerCase() + this.myQuote;
  }

  public buildVIEW(view: abaplint.Objects.View): string {
    const fields = view.getFields() || [];
    const columns = fields.map((f) => {
      return this.quote(f.TABNAME) + "." + f.FIELDNAME.toLowerCase() + " AS " + f.VIEWFIELD.toLowerCase();
    }).join(", ");

    // a DD28J row compares two fields, or without RFIELD a field to the constant in RTAB, eg. 'EN'
    const joins: Join[] = view.getJoin() || [];
    let pending = joins.filter(j => j.RFIELD !== undefined && j.RFIELD !== "");
    const constants = joins.filter(j => j.RFIELD === undefined || j.RFIELD === "");

    // start from the table of the first view field, typically the root table, then repeatedly
    // join a table that has conditions on the tables already joined, LTAB is not always the new one
    const tables: string[] = [];
    const conditions: {[table: string]: string[]} = {};
    const add = (table: string) => {
      tables.push(table);
      const on = pending.filter(j => tables.includes(j.LTAB) && tables.includes(j.RTAB));
      pending = pending.filter(j => on.includes(j) === false);
      conditions[table] = [...on, ...constants.filter(j => j.LTAB === table)].map(j => this.condition(view, j));
    };
    const first = fields[0]?.TABNAME;
    const root = first !== undefined && (pending.length === 0 || pending.some(j => j.LTAB === first || j.RTAB === first))
      ? first : (pending[0] || constants[0])?.LTAB;
    if (root === undefined) {
      throw new Error(`View ${view.getName()}: no tables`);
    }
    add(root);
    while (pending.length > 0) {
      // tables without conditions on the joined ones are cross joined
      const next = pending.find(j => tables.includes(j.LTAB) || tables.includes(j.RTAB)) || pending[0];
      add(tables.includes(next.LTAB) ? next.RTAB : next.LTAB);
    }
    for (const c of constants) {
      if (tables.includes(c.LTAB) === false) {
        add(c.LTAB);
      }
    }

    let from = this.quote(tables[0]);
    for (const table of tables.slice(1)) {
      from += conditions[table].length === 0
        ? " CROSS JOIN " + this.quote(table)
        : " INNER JOIN " + this.quote(table) + " ON " + conditions[table].join(" AND ");
    }
    if (conditions[tables[0]].length > 0) {
      from += " WHERE " + conditions[tables[0]].join(" AND ");
    }

    return `CREATE VIEW ${this.quote(view.getName())} AS SELECT ${columns} FROM ${from};\n`;
  }

  private condition(view: abaplint.Objects.View, j: Join): string {
    const operator = OPERATORS[j.OPERATOR];
    if (operator === undefined) {
      throw new Error(`View ${view.getName()}: join operator ${j.OPERATOR} not supported`);
    }
    const right = j.RFIELD === undefined || j.RFIELD === ""
      ? this.escape(j.RTAB)
      : this.quote(j.RTAB) + "." + j.RFIELD.toLowerCase();
    return this.quote(j.LTAB) + "." + j.LFIELD.toLowerCase() + " " + operator + " " + right;
  }

  // the constant keeps its single quotes, statements are inside a javascript string template
  private escape(value: string): string {
    let ret = value.replace(/\\/g, "\\\\");
    ret = ret.replace(/`/g, "\\`");
    ret = ret.replace(/\${/g, "\\${");
    return ret;
  }

}
