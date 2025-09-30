import * as abaplint from "@abaplint/core";

export class DatabaseSchemaReuse {
  private readonly myQuote: string;

  public constructor(quote: string) {
    this.myQuote = quote;
  }

  private quote(name: string): string {
    return this.myQuote + name.toLowerCase() + this.myQuote;
  }

  public buildVIEW(view: abaplint.Objects.View): string {
    const fields = view.getFields();
    let firstTabname = "";
    const columns = fields?.map((f) => {
      firstTabname = this.quote(f.TABNAME.toLowerCase());
      return firstTabname + "." + f.FIELDNAME.toLowerCase() + " AS " + f.VIEWFIELD.toLowerCase();
    }).join(", ");

    let from = "";
    let previous = "";
    for (const j of view.getJoin() || []) {
      if (previous === "") {
        from += this.quote(j.LTAB.toLowerCase()) + " INNER JOIN " + this.quote(j.RTAB.toLowerCase()) + " ON " + this.quote(j.LTAB.toLowerCase()) + "." + j.LFIELD.toLowerCase() + " = " + this.quote(j.RTAB.toLowerCase()) + "." + j.RFIELD.toLowerCase();
      } else if (previous === j.LTAB + "," + j.RTAB) {
        from += " AND " + this.quote(j.LTAB.toLowerCase()) + "." + j.LFIELD.toLowerCase() + " = " + this.quote(j.RTAB.toLowerCase()) + "." + j.RFIELD.toLowerCase();
      } else {
        from += " INNER JOIN " + this.quote(j.RTAB.toLowerCase()) + " ON " + this.quote(j.LTAB.toLowerCase()) + "." + j.LFIELD.toLowerCase() + " = " + this.quote(j.RTAB.toLowerCase()) + "." + j.RFIELD.toLowerCase();
      }
      previous = j.LTAB + "," + j.RTAB;
    }
    from = from.trim();
    if (from === "") {
      from = firstTabname;
    }

    return `CREATE VIEW ${this.quote(view.getName().toLowerCase())} AS SELECT ${columns} FROM ${from};\n`;
  }

}