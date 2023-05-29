export const tabl_t100xml = `<?xml version="1.0" encoding="utf-8"?>
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

export const zt100_ttyp = `
<?xml version="1.0" encoding="utf-8"?>
<abapGit version="v1.0.0" serializer="LCL_OBJECT_TTYP" serializer_version="v1.0.0">
 <asx:abap xmlns:asx="http://www.sap.com/abapxml" version="1.0">
  <asx:values>
   <DD40V>
    <TYPENAME>ZT100_TTYP</TYPENAME>
    <DDLANGUAGE>E</DDLANGUAGE>
    <ROWTYPE>T100</ROWTYPE>
    <ROWKIND>S</ROWKIND>
    <DATATYPE>STRU</DATATYPE>
    <ACCESSMODE>T</ACCESSMODE>
    <KEYDEF>D</KEYDEF>
    <KEYKIND>N</KEYKIND>
    <DDTEXT>ttyp</DDTEXT>
   </DD40V>
  </asx:values>
 </asx:abap>
</abapGit>`;

export const msag_zag_unit_test = `<?xml version="1.0" encoding="utf-8"?>
<abapGit version="v1.0.0" serializer="LCL_OBJECT_MSAG" serializer_version="v1.0.0">
 <asx:abap xmlns:asx="http://www.sap.com/abapxml" version="1.0">
  <asx:values>
   <T100A>
    <ARBGB>ZAG_UNIT_TEST</ARBGB>
    <MASTERLANG>E</MASTERLANG>
    <STEXT>test</STEXT>
   </T100A>
   <T100>
    <T100>
     <SPRSL>E</SPRSL>
     <ARBGB>ZAG_UNIT_TEST</ARBGB>
     <MSGNR>000</MSGNR>
     <TEXT>hello world</TEXT>
    </T100>
    <T100>
     <SPRSL>E</SPRSL>
     <ARBGB>ZAG_UNIT_TEST</ARBGB>
     <MSGNR>123</MSGNR>
     <TEXT>blah</TEXT>
    </T100>
   </T100>
  </asx:values>
 </asx:abap>
</abapGit>`;

export const msag_escape = `<?xml version="1.0" encoding="utf-8"?>
<abapGit version="v1.0.0" serializer="LCL_OBJECT_MSAG" serializer_version="v1.0.0">
 <asx:abap xmlns:asx="http://www.sap.com/abapxml" version="1.0">
  <asx:values>
   <T100A>
    <ARBGB>ZESCAPE</ARBGB>
    <MASTERLANG>E</MASTERLANG>
    <STEXT>test</STEXT>
   </T100A>
   <T100>
    <T100>
     <SPRSL>E</SPRSL>
     <ARBGB>ZESCAPE</ARBGB>
     <MSGNR>000</MSGNR>
     <TEXT>FOO 'HELLO' bar</TEXT>
    </T100>
   </T100>
  </asx:values>
 </asx:abap>
</abapGit>`;

export const zt111 = `<?xml version="1.0" encoding="utf-8"?>
<abapGit version="v1.0.0" serializer="LCL_OBJECT_TABL" serializer_version="v1.0.0">
 <asx:abap xmlns:asx="http://www.sap.com/abapxml" version="1.0">
  <asx:values>
   <DD02V>
    <TABNAME>ZT111</TABNAME>
    <DDLANGUAGE>E</DDLANGUAGE>
    <TABCLASS>TRANSP</TABCLASS>
    <LANGDEP>X</LANGDEP>
    <DDTEXT>test</DDTEXT>
    <CONTFLAG>A</CONTFLAG>
    <EXCLASS>1</EXCLASS>
   </DD02V>
   <DD09L>
    <TABNAME>ZT111</TABNAME>
    <AS4LOCAL>A</AS4LOCAL>
    <TABKAT>0</TABKAT>
    <TABART>APPL0</TABART>
    <BUFALLOW>N</BUFALLOW>
   </DD09L>
   <DD03P_TABLE>
    <DD03P>
     <FIELDNAME>.INCLUDE</FIELDNAME>
     <KEYFLAG>X</KEYFLAG>
     <ADMINFIELD>0</ADMINFIELD>
     <PRECFIELD>ZT222</PRECFIELD>
     <NOTNULL>X</NOTNULL>
     <MASK>      S</MASK>
     <DDTEXT>sdfsd</DDTEXT>
     <COMPTYPE>S</COMPTYPE>
     <GROUPNAME>FOOGROUP</GROUPNAME>
    </DD03P>
   </DD03P_TABLE>
  </asx:values>
 </asx:abap>
</abapGit>`;

export const zt222 = `<?xml version="1.0" encoding="utf-8"?>
<abapGit version="v1.0.0" serializer="LCL_OBJECT_TABL" serializer_version="v1.0.0">
 <asx:abap xmlns:asx="http://www.sap.com/abapxml" version="1.0">
  <asx:values>
   <DD02V>
    <TABNAME>ZT222</TABNAME>
    <DDLANGUAGE>E</DDLANGUAGE>
    <TABCLASS>INTTAB</TABCLASS>
    <DDTEXT>sdfsd</DDTEXT>
    <EXCLASS>1</EXCLASS>
   </DD02V>
   <DD03P_TABLE>
    <DD03P>
     <FIELDNAME>TEST</FIELDNAME>
     <ADMINFIELD>0</ADMINFIELD>
     <INTTYPE>C</INTTYPE>
     <INTLEN>000004</INTLEN>
     <DATATYPE>CHAR</DATATYPE>
     <LENG>000002</LENG>
     <MASK>  CHAR</MASK>
    </DD03P>
    <DD03P>
     <FIELDNAME>SDF</FIELDNAME>
     <ADMINFIELD>0</ADMINFIELD>
     <INTTYPE>C</INTTYPE>
     <INTLEN>000006</INTLEN>
     <DATATYPE>CHAR</DATATYPE>
     <LENG>000003</LENG>
     <MASK>  CHAR</MASK>
    </DD03P>
   </DD03P_TABLE>
  </asx:values>
 </asx:abap>
</abapGit>`;

export const zquan = `<?xml version="1.0" encoding="utf-8"?>
<abapGit version="v1.0.0" serializer="LCL_OBJECT_TABL" serializer_version="v1.0.0">
 <asx:abap xmlns:asx="http://www.sap.com/abapxml" version="1.0">
  <asx:values>
   <DD02V>
    <TABNAME>ZQUAN</TABNAME>
    <DDLANGUAGE>E</DDLANGUAGE>
    <TABCLASS>TRANSP</TABCLASS>
    <DDTEXT>QUAN</DDTEXT>
    <CONTFLAG>A</CONTFLAG>
    <EXCLASS>1</EXCLASS>
   </DD02V>
   <DD09L>
    <TABNAME>ZQUAN</TABNAME>
    <AS4LOCAL>A</AS4LOCAL>
    <TABKAT>0</TABKAT>
    <TABART>APPL0</TABART>
    <BUFALLOW>N</BUFALLOW>
   </DD09L>
   <DD03P_TABLE>
    <DD03P>
     <FIELDNAME>KEYFIELD</FIELDNAME>
     <KEYFLAG>X</KEYFLAG>
     <ADMINFIELD>0</ADMINFIELD>
     <INTTYPE>C</INTTYPE>
     <INTLEN>000020</INTLEN>
     <NOTNULL>X</NOTNULL>
     <DATATYPE>CHAR</DATATYPE>
     <LENG>000010</LENG>
     <MASK>  CHAR</MASK>
    </DD03P>
    <DD03P>
     <FIELDNAME>VALUEFIELD</FIELDNAME>
     <ADMINFIELD>0</ADMINFIELD>
     <INTTYPE>P</INTTYPE>
     <INTLEN>000006</INTLEN>
     <REFTABLE>MARA</REFTABLE>
     <REFFIELD>MEINS</REFFIELD>
     <DATATYPE>QUAN</DATATYPE>
     <LENG>000010</LENG>
     <DECIMALS>000002</DECIMALS>
     <MASK>  QUAN</MASK>
    </DD03P>
   </DD03P_TABLE>
  </asx:values>
 </asx:abap>
</abapGit>`;