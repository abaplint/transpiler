import * as abaplint from "@abaplint/core";

export class DatabaseSchemaReuse {

  public static buildVIEW(view: abaplint.Objects.View, quote: string): string {
    const fields = view.getFields();
    let firstTabname = "";
    const columns = fields?.map((f) => {
      firstTabname = quote + f.TABNAME.toLowerCase() + quote;
      return firstTabname + "." + f.FIELDNAME.toLowerCase() + " AS " + f.VIEWFIELD.toLowerCase();
    }).join(", ");

    let from = "";
    let previous = "";
    for (const j of view.getJoin() || []) {
      if (previous === "") {
        from += quote + j.LTAB.toLowerCase() + quote + " INNER JOIN " + quote + j.RTAB.toLowerCase() + quote + " ON " + quote + j.LTAB.toLowerCase() + quote + "." + j.LFIELD.toLowerCase() + " = " + quote + j.RTAB.toLowerCase() + quote + "." + j.RFIELD.toLowerCase();
      } else if (previous === j.LTAB + "," + j.RTAB) {
        from += " AND " + quote + j.LTAB.toLowerCase() + quote + "." + j.LFIELD.toLowerCase() + " = " + quote + j.RTAB.toLowerCase() + quote + "." + j.RFIELD.toLowerCase();
      } else {
        from += " INNER JOIN " + quote + j.RTAB.toLowerCase() + quote + " ON " + quote + j.LTAB.toLowerCase() + quote + "." + j.LFIELD.toLowerCase() + " = " + quote + j.RTAB.toLowerCase() + quote + "." + j.RFIELD.toLowerCase();
      }
      previous = j.LTAB + "," + j.RTAB;
    }
    from = from.trim();
    if (from === "") {
      from = firstTabname;
    }

    return `CREATE VIEW ${quote}${view.getName().toLowerCase()}${quote} AS SELECT ${columns} FROM ${from};\n`;
  }

}