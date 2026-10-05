import {expect} from "chai";
import {Transpiler} from "../src";

const tabl = `<?xml version="1.0" encoding="utf-8"?>
<abapGit version="v1.0.0" serializer="LCL_OBJECT_TABL" serializer_version="v1.0.0">
 <asx:abap xmlns:asx="http://www.sap.com/abapxml" version="1.0">
  <asx:values>
   <DD02V><TABNAME>ZINT8</TABNAME><TABCLASS>TRANSP</TABCLASS></DD02V>
   <DD03P_TABLE>
    <DD03P><FIELDNAME>ID</FIELDNAME><KEYFLAG>X</KEYFLAG><INTTYPE>C</INTTYPE><INTLEN>000008</INTLEN>
     <DATATYPE>CHAR</DATATYPE><LENG>000004</LENG></DD03P>
    <DD03P><FIELDNAME>AMOUNT</FIELDNAME><INTTYPE>8</INTTYPE><INTLEN>000008</INTLEN>
     <DATATYPE>INT8</DATATYPE><LENG>000019</LENG></DD03P>
   </DD03P_TABLE>
  </asx:values>
 </asx:abap>
</abapGit>`;

describe("Database schema INT8", () => {
  it("maps INT8 for SQLite, PostgreSQL, and Snowflake", async () => {
    const output = await new Transpiler().runRaw([{filename: "zint8.tabl.xml", contents: tabl}]);
    expect(output.databaseSetup.schemas.sqlite.join(" ")).to.include("'amount' INTEGER");
    expect(output.databaseSetup.schemas.pg.join(" ")).to.include('"amount" BIGINT');
    expect(output.databaseSetup.schemas.snowflake.join(" ")).to.include('"amount" NUMBER(19)');
  });
});
