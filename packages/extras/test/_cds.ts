export const t000 = `<?xml version="1.0" encoding="utf-8"?>
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

export const joinedView = `@EndUserText.label: 'Joined view'
define view entity ZDDLS as select from t000 as item
  inner join t001 as header on item.mandt = header.mandt
  left outer join t002 as text on header.mandt = text.mandt
    and (text.cccategory = 'E' or text.cccategory = 'F')
  left outer join t003 as change on item.mandt = change.mandt
{
  key item.mandt as PurchasingDocument,
  @EndUserText.label: 'Category'
  header.cccategory as HeaderCategory,
  text.cccategory as Language,
  change.cccategory as ChangeStatus
}
where item.mandt <> '999'`;

export function viewFiles(ddls = joinedView): {filename: string, contents: string}[] {
  return [
    ...["t000", "t001", "t002", "t003"].map(name => ({
      filename: name + ".tabl.xml",
      contents: t000.replace(/T000/g, name.toUpperCase()),
    })),
    {filename: "zddls.ddls.asddls", contents: ddls},
  ];
}

