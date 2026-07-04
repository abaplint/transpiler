import {expect} from "chai";
import * as abaplint from "@abaplint/core";
import {DatabaseSetupResult, IOutputFile, ITranspilerOptions, ITranspilerPlugin, Transpiler} from "@abaplint/transpiler";
import {plugin} from "../src";

const t000 = `<?xml version="1.0" encoding="utf-8"?>
<abapGit version="v1.0.0" serializer="LCL_OBJECT_TABL" serializer_version="v1.0.0">
 <asx:abap xmlns:asx="http://www.sap.com/abapxml" version="1.0">
  <asx:values>
   <DD02V>
    <TABNAME>T000</TABNAME>
    <DDLANGUAGE>E</DDLANGUAGE>
    <TABCLASS>TRANSP</TABCLASS>
    <DDTEXT>T000</DDTEXT>
    <CONTFLAG>A</CONTFLAG>
    <EXCLASS>1</EXCLASS>
   </DD02V>
   <DD09L>
    <TABNAME>T000</TABNAME>
    <AS4LOCAL>A</AS4LOCAL>
    <TABKAT>0</TABKAT>
    <TABART>APPL0</TABART>
    <BUFALLOW>N</BUFALLOW>
   </DD09L>
   <DD03P_TABLE>
    <DD03P>
     <TABNAME>T000</TABNAME>
     <FIELDNAME>MANDT</FIELDNAME>
     <DDLANGUAGE>E</DDLANGUAGE>
     <POSITION>0001</POSITION>
     <KEYFLAG>X</KEYFLAG>
     <ADMINFIELD>0</ADMINFIELD>
     <INTTYPE>C</INTTYPE>
     <INTLEN>000006</INTLEN>
     <NOTNULL>X</NOTNULL>
     <DATATYPE>CHAR</DATATYPE>
     <LENG>000003</LENG>
     <MASK>  CHAR</MASK>
    </DD03P>
    <DD03P>
     <TABNAME>T000</TABNAME>
     <FIELDNAME>CCCATEGORY</FIELDNAME>
     <DDLANGUAGE>E</DDLANGUAGE>
     <POSITION>0002</POSITION>
     <ADMINFIELD>0</ADMINFIELD>
     <INTTYPE>C</INTTYPE>
     <INTLEN>000002</INTLEN>
     <DATATYPE>CHAR</DATATYPE>
     <LENG>000001</LENG>
     <MASK>  CHAR</MASK>
    </DD03P>
   </DD03P_TABLE>
  </asx:values>
 </asx:abap>
</abapGit>`;

class AmendDatabase implements ITranspilerPlugin {
  public objectTypes(): string[] {
    return [];
  }

  public handleObject(_obj: abaplint.IObject, _reg: abaplint.IRegistry, _options: ITranspilerOptions): IOutputFile[] | undefined {
    return undefined;
  }

  public amendDatabaseSetup(dbSetup: DatabaseSetupResult, _reg: abaplint.IRegistry, _options: ITranspilerOptions): void {
    dbSetup.schemas.sqlite.push("CREATE TABLE zplugin (foo NCHAR(1));");
    dbSetup.insert.push("INSERT INTO zplugin VALUES ('A');");
  }
}

describe("amend database setup", () => {

  it("plugin schema and insert statements are added", async () => {
    const reg = new abaplint.Registry().parse();

    const res = await new Transpiler({}, new AmendDatabase()).run(reg);

    expect(res.databaseSetup.schemas.sqlite.join("\n")).to.include("CREATE TABLE zplugin");
    expect(res.databaseSetup.insert.join("\n")).to.include("INSERT INTO zplugin");
    expect(res.initializationScript).to.include("CREATE TABLE zplugin");
  });

});

describe("DDLS database setup", () => {

  it("creates a database view on top of a table", async () => {
    const ddls = `define view entity ZDDLS as select from t000 {
  key mandt,
      cccategory
}`;
    const reg = new abaplint.Registry()
      .addFile(new abaplint.MemoryFile("t000.tabl.xml", t000))
      .addFile(new abaplint.MemoryFile("zddls.ddls.asddls", ddls))
      .parse();

    const res = await new Transpiler({}, plugin).run(reg);
    const sqlite = res.databaseSetup.schemas.sqlite.join("\n");

    expect(sqlite).to.include(
      "CREATE VIEW 'zddls' AS SELECT 't000'.'mandt' AS mandt, " +
      "'t000'.'cccategory' AS cccategory FROM 't000';");
  });

});
