import * as abaplint from "@abaplint/core";

export type TableColumn = {name: string, type: abaplint.AbstractType};

/** External view, VIEWCLASS X: a DDIC view on a HANA view, eg. a calculation view. Its definition lives
 * in HANA, so locally it becomes a table with the declared columns which tests can fill with data */
export class ExternalView {

  public static isExternal(view: abaplint.Objects.View): boolean {
    return /<VIEWCLASS>X<\/VIEWCLASS>/.test(view.getXMLFile()?.getRaw() || "");
  }

  public static columns(view: abaplint.Objects.View, reg: abaplint.IRegistry): {columns: TableColumn[], keys: string[]} {
    const type = view.parseType(reg);
    if (!(type instanceof abaplint.BasicTypes.StructureType)) {
      throw new Error(`External view ${view.getName()}: no fields`);
    }
    // the field types come from EXVDATATYPE, abaplint types the fields void if it is missing
    return {columns: [...type.getComponents()], keys: view.listKeys()};
  }

}
