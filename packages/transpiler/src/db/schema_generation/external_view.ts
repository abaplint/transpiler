import * as abaplint from "@abaplint/core";

export type TableColumn = {name: string, type: abaplint.AbstractType};

/** External view, VIEWCLASS X: a DDIC view on a HANA view, eg. a calculation view. Its definition lives
 * in HANA, so locally it becomes a table with the declared columns which tests can fill with data */
export class ExternalView {

  public static isExternal(view: abaplint.Objects.View): boolean {
    return /<VIEWCLASS>X<\/VIEWCLASS>/.test(view.getXMLFile()?.getRaw() || "");
  }

  public static columns(view: abaplint.Objects.View): {columns: TableColumn[], keys: string[]} {
    const raw = view.getXMLFile()?.getRaw() || "";
    const columns: TableColumn[] = [];
    const keys: string[] = [];
    for (const match of raw.matchAll(/<DD27P>([\s\S]*?)<\/DD27P>/g)) {
      const value = (tag: string) => new RegExp(`<${tag}>([^<]*)</${tag}>`).exec(match[1])?.[1];
      const name = value("VIEWFIELD");
      if (name === undefined) {
        continue;
      }
      const type = ExternalView.toType(value("EXVDATATYPE"), parseInt(value("EXVFLENGTH") || "0", 10),
                                       parseInt(value("EXVDECIMALS") || "0", 10));
      if (type === undefined) {
        throw new Error(`External view ${view.getName()}-${name}: type ${value("EXVDATATYPE")} not supported`);
      }
      columns.push({name, type});
      if (value("KEYFLAG") === "X") {
        keys.push(name);
      }
    }
    return {columns, keys};
  }

  private static toType(datatype: string | undefined, length: number, decimals: number): abaplint.AbstractType | undefined {
    switch (datatype) {
      case "CHAR":
      case "CLNT":
      case "CUKY":
      case "UNIT":
      case "LANG":
        return new abaplint.BasicTypes.CharacterType(length);
      case "NUMC":
        return new abaplint.BasicTypes.NumericType(length);
      case "DATS":
        return new abaplint.BasicTypes.DateType();
      case "TIMS":
        return new abaplint.BasicTypes.TimeType();
      case "INT1":
      case "INT2":
      case "INT4":
        return abaplint.BasicTypes.IntegerType.get();
      case "INT8":
        return new abaplint.BasicTypes.Integer8Type();
      case "DEC":
      case "CURR":
      case "QUAN":
        // EXVFLENGTH is the number of digits
        return new abaplint.BasicTypes.PackedType(Math.floor(length / 2) + 1, decimals);
      case "FLTP":
        return new abaplint.BasicTypes.FloatType();
      case "STRING":
      case "SSTRING":
        return abaplint.BasicTypes.StringType.get();
      case "RAW":
        return new abaplint.BasicTypes.HexType(length);
      case "RAWSTRING":
        return abaplint.BasicTypes.XStringType.get();
      default:
        return undefined;
    }
  }

}
