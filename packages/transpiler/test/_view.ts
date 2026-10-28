// ZV_HEAD_TEXT joins ZTEXT to ZHEAD with the constant condition ZTEXT-LANGU = 'EN',
// and ZINFO to ZTEXT, so the third table is joined to the second, not the first

function tabl(name: string, fields: {name: string, length: number, key?: boolean}[]): string {
  const dd03p = fields.map(f => `    <DD03P>
     <FIELDNAME>${f.name}</FIELDNAME>${f.key === true ? "\n     <KEYFLAG>X</KEYFLAG>" : ""}
     <INTTYPE>C</INTTYPE>
     <INTLEN>${String(f.length * 2).padStart(6, "0")}</INTLEN>${f.key === true ? "\n     <NOTNULL>X</NOTNULL>" : ""}
     <DATATYPE>CHAR</DATATYPE>
     <LENG>${String(f.length).padStart(6, "0")}</LENG>
    </DD03P>`).join("\n");
  return `<?xml version="1.0" encoding="utf-8"?>
<abapGit version="v1.0.0" serializer="LCL_OBJECT_TABL" serializer_version="v1.0.0">
 <asx:abap xmlns:asx="http://www.sap.com/abapxml" version="1.0">
  <asx:values>
   <DD02V>
    <TABNAME>${name}</TABNAME>
    <DDLANGUAGE>E</DDLANGUAGE>
    <TABCLASS>TRANSP</TABCLASS>
    <CLIDEP>X</CLIDEP>
    <CONTFLAG>A</CONTFLAG>
   </DD02V>
   <DD03P_TABLE>
${dd03p}
   </DD03P_TABLE>
  </asx:values>
 </asx:abap>
</abapGit>`;
}

export const zhead = tabl("ZHEAD", [
  {name: "MANDT", length: 3, key: true},
  {name: "ID", length: 10, key: true},
]);

export const ztext = tabl("ZTEXT", [
  {name: "MANDT", length: 3, key: true},
  {name: "ID", length: 10, key: true},
  {name: "LANGU", length: 2, key: true},
  {name: "DESCR", length: 40},
]);

export const zinfo = tabl("ZINFO", [
  {name: "MANDT", length: 3, key: true},
  {name: "ID", length: 10, key: true},
  {name: "INFO", length: 40},
]);

export function zvHeadText(constant = "&apos;EN&apos;"): string {
  return `<?xml version="1.0" encoding="utf-8"?>
<abapGit version="v1.0.0" serializer="LCL_OBJECT_VIEW" serializer_version="v1.0.0">
 <asx:abap xmlns:asx="http://www.sap.com/abapxml" version="1.0">
  <asx:values>
   <DD25V>
    <VIEWNAME>ZV_HEAD_TEXT</VIEWNAME>
    <AS4LOCAL>A</AS4LOCAL>
    <AGGTYPE>V</AGGTYPE>
    <ROOTTAB>ZHEAD</ROOTTAB>
    <VIEWCLASS>D</VIEWCLASS>
   </DD25V>
   <DD26V_TABLE>
    <DD26V><VIEWNAME>ZV_HEAD_TEXT</VIEWNAME><TABNAME>ZHEAD</TABNAME><TABPOS>0001</TABPOS><FORTABNAME>ZHEAD</FORTABNAME></DD26V>
    <DD26V><VIEWNAME>ZV_HEAD_TEXT</VIEWNAME><TABNAME>ZTEXT</TABNAME>
     <TABPOS>0002</TABPOS><FORTABNAME>ZHEAD</FORTABNAME><FORFIELD>ID</FORFIELD><FORDIR>E</FORDIR></DD26V>
    <DD26V><VIEWNAME>ZV_HEAD_TEXT</VIEWNAME><TABNAME>ZINFO</TABNAME>
     <TABPOS>0003</TABPOS><FORTABNAME>ZTEXT</FORTABNAME><FORFIELD>ID</FORFIELD><FORDIR>E</FORDIR></DD26V>
   </DD26V_TABLE>
   <DD27P_TABLE>
    <DD27P><VIEWFIELD>MANDT</VIEWFIELD><TABNAME>ZHEAD</TABNAME><FIELDNAME>MANDT</FIELDNAME><KEYFLAG>X</KEYFLAG></DD27P>
    <DD27P><VIEWFIELD>ID</VIEWFIELD><TABNAME>ZHEAD</TABNAME><FIELDNAME>ID</FIELDNAME><KEYFLAG>X</KEYFLAG></DD27P>
    <DD27P><VIEWFIELD>DESCR</VIEWFIELD><TABNAME>ZTEXT</TABNAME><FIELDNAME>DESCR</FIELDNAME></DD27P>
    <DD27P><VIEWFIELD>INFO</VIEWFIELD><TABNAME>ZINFO</TABNAME><FIELDNAME>INFO</FIELDNAME></DD27P>
   </DD27P_TABLE>
   <DD28J_TABLE>
    <DD28J><VIEWNAME>ZV_HEAD_TEXT</VIEWNAME><LTAB>ZTEXT</LTAB><LFIELD>MANDT</LFIELD>
     <OPERATOR>EQ</OPERATOR><RTAB>ZHEAD</RTAB><RFIELD>MANDT</RFIELD><SOURCE>F</SOURCE></DD28J>
    <DD28J><VIEWNAME>ZV_HEAD_TEXT</VIEWNAME><LTAB>ZTEXT</LTAB><LFIELD>ID</LFIELD>
     <OPERATOR>EQ</OPERATOR><RTAB>ZHEAD</RTAB><RFIELD>ID</RFIELD><SOURCE>F</SOURCE></DD28J>
    <DD28J><VIEWNAME>ZV_HEAD_TEXT</VIEWNAME><LTAB>ZTEXT</LTAB><LFIELD>LANGU</LFIELD>
     <OPERATOR>EQ</OPERATOR><RTAB>${constant}</RTAB><SOURCE>F</SOURCE></DD28J>
    <DD28J><VIEWNAME>ZV_HEAD_TEXT</VIEWNAME><LTAB>ZINFO</LTAB><LFIELD>MANDT</LFIELD>
     <OPERATOR>EQ</OPERATOR><RTAB>ZTEXT</RTAB><RFIELD>MANDT</RFIELD><SOURCE>F</SOURCE></DD28J>
    <DD28J><VIEWNAME>ZV_HEAD_TEXT</VIEWNAME><LTAB>ZINFO</LTAB><LFIELD>ID</LFIELD>
     <OPERATOR>EQ</OPERATOR><RTAB>ZTEXT</RTAB><RFIELD>ID</RFIELD><SOURCE>F</SOURCE></DD28J>
   </DD28J_TABLE>
  </asx:values>
 </asx:abap>
</abapGit>`;
}
