import {expect} from "chai";
import {Transpiler} from "../src";

const tabl = `<?xml version="1.0" encoding="utf-8"?>
<abapGit version="v1.0.0" serializer="LCL_OBJECT_TABL" serializer_version="v1.0.0">
 <asx:abap xmlns:asx="http://www.sap.com/abapxml" version="1.0">
  <asx:values>
   <DD02V><TABNAME>ZSETUP_T</TABNAME><TABCLASS>TRANSP</TABCLASS></DD02V>
   <DD03P_TABLE>
    <DD03P><FIELDNAME>ID</FIELDNAME><KEYFLAG>X</KEYFLAG><INTTYPE>C</INTTYPE><INTLEN>000008</INTLEN>
     <DATATYPE>CHAR</DATATYPE><LENG>000004</LENG></DD03P>
    <DD03P><FIELDNAME>NAME</FIELDNAME><INTTYPE>C</INTTYPE><INTLEN>000020</INTLEN>
     <DATATYPE>CHAR</DATATYPE><LENG>000010</LENG></DD03P>
   </DD03P_TABLE>
  </asx:values>
 </asx:abap>
</abapGit>`;

const view = `<?xml version="1.0" encoding="utf-8"?>
<abapGit version="v1.0.0" serializer="LCL_OBJECT_VIEW" serializer_version="v1.0.0">
 <asx:abap xmlns:asx="http://www.sap.com/abapxml" version="1.0">
  <asx:values>
   <DD25V><VIEWNAME>ZSETUP_V</VIEWNAME><AGGTYPE>V</AGGTYPE><ROOTTAB>ZSETUP_T</ROOTTAB><VIEWCLASS>D</VIEWCLASS></DD25V>
   <DD26V_TABLE>
    <DD26V><VIEWNAME>ZSETUP_V</VIEWNAME><TABNAME>ZSETUP_T</TABNAME><TABPOS>0001</TABPOS><FORTABNAME>ZSETUP_T</FORTABNAME></DD26V>
   </DD26V_TABLE>
   <DD27P_TABLE>
    <DD27P><VIEWFIELD>ID</VIEWFIELD><TABNAME>ZSETUP_T</TABNAME><FIELDNAME>ID</FIELDNAME><KEYFLAG>X</KEYFLAG></DD27P>
    <DD27P><VIEWFIELD>NAME</VIEWFIELD><TABNAME>ZSETUP_T</TABNAME><FIELDNAME>NAME</FIELDNAME></DD27P>
   </DD27P_TABLE>
  </asx:values>
 </asx:abap>
</abapGit>`;

const tadir = `<?xml version="1.0" encoding="utf-8"?>
<abapGit version="v1.0.0" serializer="LCL_OBJECT_TABL" serializer_version="v1.0.0">
 <asx:abap xmlns:asx="http://www.sap.com/abapxml" version="1.0">
  <asx:values>
   <DD02V><TABNAME>TADIR</TABNAME><TABCLASS>TRANSP</TABCLASS></DD02V>
   <DD03P_TABLE>
    <DD03P><FIELDNAME>PGMID</FIELDNAME><KEYFLAG>X</KEYFLAG><INTTYPE>C</INTTYPE><INTLEN>000008</INTLEN>
     <DATATYPE>CHAR</DATATYPE><LENG>000004</LENG></DD03P>
    <DD03P><FIELDNAME>OBJECT</FIELDNAME><KEYFLAG>X</KEYFLAG><INTTYPE>C</INTTYPE><INTLEN>000008</INTLEN>
     <DATATYPE>CHAR</DATATYPE><LENG>000004</LENG></DD03P>
    <DD03P><FIELDNAME>OBJ_NAME</FIELDNAME><KEYFLAG>X</KEYFLAG><INTTYPE>C</INTTYPE><INTLEN>000080</INTLEN>
     <DATATYPE>CHAR</DATATYPE><LENG>000040</LENG></DD03P>
    <DD03P><FIELDNAME>DEVCLASS</FIELDNAME><INTTYPE>C</INTTYPE><INTLEN>000060</INTLEN><DATATYPE>CHAR</DATATYPE><LENG>000030</LENG></DD03P>
    <DD03P><FIELDNAME>KORRNUM</FIELDNAME><INTTYPE>C</INTTYPE><INTLEN>000020</INTLEN><DATATYPE>CHAR</DATATYPE><LENG>000010</LENG></DD03P>
    <DD03P><FIELDNAME>SRCSYSTEM</FIELDNAME><INTTYPE>C</INTTYPE><INTLEN>000020</INTLEN><DATATYPE>CHAR</DATATYPE><LENG>000010</LENG></DD03P>
    <DD03P><FIELDNAME>DELFLAG</FIELDNAME><INTTYPE>C</INTTYPE><INTLEN>000002</INTLEN><DATATYPE>CHAR</DATATYPE><LENG>000001</LENG></DD03P>
    <DD03P><FIELDNAME>GENFLAG</FIELDNAME><INTTYPE>C</INTTYPE><INTLEN>000002</INTLEN><DATATYPE>CHAR</DATATYPE><LENG>000001</LENG></DD03P>
    <DD03P><FIELDNAME>EDTFLAG</FIELDNAME><INTTYPE>C</INTTYPE><INTLEN>000002</INTLEN><DATATYPE>CHAR</DATATYPE><LENG>000001</LENG></DD03P>
    <DD03P><FIELDNAME>MASTERLANG</FIELDNAME><INTTYPE>C</INTTYPE><INTLEN>000002</INTLEN><DATATYPE>CHAR</DATATYPE><LENG>000001</LENG></DD03P>
   </DD03P_TABLE>
  </asx:values>
 </asx:abap>
</abapGit>`;

const files = [
  {filename: "zsetup_t.tabl.xml", contents: tabl},
  {filename: "zsetup_v.view.xml", contents: view},
  {filename: "tadir.tabl.xml", contents: tadir},
];

describe("Database setup, per table result", () => {
  it("hands out one entry per table and view, the same SQL as the flat lists", async () => {
    const setup = (await new Transpiler().runRaw(files)).databaseSetup;

    expect(setup.tables.map(t => t.type + " " + t.name)).to.deep.equal(["TABL ZSETUP_T", "TABL TADIR", "VIEW ZSETUP_V"]);
    expect(setup.schemas.sqlite.length).to.equal(setup.tables.length);
    for (const [i, t] of setup.tables.entries()) {
      expect(t.sqlite).to.equal(setup.schemas.sqlite[i]);
      expect(t.pg).to.equal(setup.schemas.pg[i]);
      expect(t.hdb).to.equal(setup.schemas.hdb[i]);
      expect(t.snowflake).to.equal(setup.schemas.snowflake[i]);
    }
    expect(setup.tables[0].sqlite).to.match(/^CREATE TABLE 'zsetup_t' \(/);
    expect(setup.tables[2].sqlite).to.match(/^CREATE VIEW 'zsetup_v' AS SELECT /);
    expect(setup.tables[2].pg).to.match(/^CREATE VIEW "zsetup_v" AS SELECT /);
  });

  it("names the table each INSERT targets", async () => {
    const setup = (await new Transpiler().runRaw(files)).databaseSetup;

    expect(setup.inserts.length).to.equal(3);
    expect(setup.inserts.map(i => i.sql)).to.deep.equal(setup.insert);
    for (const i of setup.inserts) {
      expect(i.table).to.equal("TADIR");
      expect(i.sql).to.include(`INSERT INTO "tadir"`);
    }
  });

  it("ifNotExists changes the prefix where the dialect accepts it", async () => {
    const setup = (await new Transpiler({databaseSetup: {ifNotExists: true}}).runRaw(files)).databaseSetup;

    const table = setup.tables[0];
    expect(table.sqlite).to.match(/^CREATE TABLE IF NOT EXISTS 'zsetup_t' \(/);
    expect(table.pg).to.match(/^CREATE TABLE IF NOT EXISTS "zsetup_t" \(/);
    expect(table.snowflake).to.match(/^CREATE TABLE IF NOT EXISTS "zsetup_t" \(/);
    expect(table.hdb).to.match(/^CREATE TABLE "zsetup_t" \(/);
    expect(setup.schemas.sqlite[0]).to.equal(table.sqlite);
    expect(setup.schemas.pg[0]).to.equal(table.pg);
    expect(setup.schemas.hdb[0]).to.equal(table.hdb);

    const view = setup.tables[2];
    expect(view.sqlite).to.match(/^CREATE VIEW IF NOT EXISTS 'zsetup_v' AS /);
    expect(view.pg).to.match(/^CREATE VIEW "zsetup_v" AS /);
  });
});
