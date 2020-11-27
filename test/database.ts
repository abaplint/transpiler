import {runFiles} from "./_utils";
import * as abap from "../packages/runtime/src/";

const t100xml = `<?xml version="1.0" encoding="utf-8"?>
<abapGit version="v1.0.0" serializer="LCL_OBJECT_TABL" serializer_version="v1.0.0">
 <asx:abap xmlns:asx="http://www.sap.com/abapxml" version="1.0">
  <asx:values>
   <DD02V>
    <TABNAME>T100</TABNAME>
    <DDLANGUAGE>E</DDLANGUAGE>
    <TABCLASS>TRANSP</TABCLASS>
    <DDTEXT>Messages</DDTEXT>
    <CONTFLAG>A</CONTFLAG>
    <EXCLASS>1</EXCLASS>
   </DD02V>
   <DD09L>
    <TABNAME>T100</TABNAME>
    <AS4LOCAL>A</AS4LOCAL>
    <TABKAT>0</TABKAT>
    <TABART>APPL0</TABART>
    <UEBERSETZ>N</UEBERSETZ>
    <BUFALLOW>N</BUFALLOW>
   </DD09L>
   <DD03P_TABLE>
    <DD03P>
     <TABNAME>T100</TABNAME>
     <FIELDNAME>SPRSL</FIELDNAME>
     <DDLANGUAGE>E</DDLANGUAGE>
     <POSITION>0001</POSITION>
     <KEYFLAG>X</KEYFLAG>
     <ADMINFIELD>0</ADMINFIELD>
     <INTTYPE>C</INTTYPE>
     <INTLEN>000002</INTLEN>
     <NOTNULL>X</NOTNULL>
     <DATATYPE>LANG</DATATYPE>
     <LENG>000001</LENG>
     <MASK>  LANG</MASK>
     <LANGUFLAG>X</LANGUFLAG>
    </DD03P>
    <DD03P>
     <TABNAME>T100</TABNAME>
     <FIELDNAME>ARBGB</FIELDNAME>
     <DDLANGUAGE>E</DDLANGUAGE>
     <POSITION>0002</POSITION>
     <KEYFLAG>X</KEYFLAG>
     <ADMINFIELD>0</ADMINFIELD>
     <INTTYPE>C</INTTYPE>
     <INTLEN>000040</INTLEN>
     <NOTNULL>X</NOTNULL>
     <DATATYPE>CHAR</DATATYPE>
     <LENG>000020</LENG>
     <MASK>  CHAR</MASK>
    </DD03P>
    <DD03P>
     <TABNAME>T100</TABNAME>
     <FIELDNAME>MSGNR</FIELDNAME>
     <DDLANGUAGE>E</DDLANGUAGE>
     <POSITION>0003</POSITION>
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
     <TABNAME>T100</TABNAME>
     <FIELDNAME>TEXT</FIELDNAME>
     <DDLANGUAGE>E</DDLANGUAGE>
     <POSITION>0004</POSITION>
     <ADMINFIELD>0</ADMINFIELD>
     <INTTYPE>C</INTTYPE>
     <INTLEN>000146</INTLEN>
     <DATATYPE>CHAR</DATATYPE>
     <LENG>000073</LENG>
     <MASK>  CHAR</MASK>
    </DD03P>
   </DD03P_TABLE>
  </asx:values>
 </asx:abap>
</abapGit>`;

describe("Top level, Database", () => {

  it("SELECT, no result", async () => {
    const code = `
    DATA ls_result TYPE t100.
    SELECT SINGLE * FROM t100 INTO ls_result.`;
    const js = await runFiles([
      {filename: "zfoobar.prog.abap", contents: code},
      {filename: "t100.tabl.xml", contents: t100xml}]);
    const f = new Function("abap", js);
    f(abap);
  });

});