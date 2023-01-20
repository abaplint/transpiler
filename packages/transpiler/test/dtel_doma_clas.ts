import {expect} from "chai";
import {Transpiler} from "../src";
import * as abaplint from "@abaplint/core";
import {IFile} from "../src/types";

async function runFiles(files: IFile[]) {
  const memory = files.map(f => new abaplint.MemoryFile(f.filename, f.contents));
  const reg: abaplint.IRegistry = new abaplint.Registry().addFiles(memory).parse();
  const res = await new Transpiler().run(reg);
  return res.objects;
}

describe("DTEL + DOMA + CLAS", () => {

  it("Testing qualified names", async () => {
    const amoo_dtel = `<?xml version="1.0" encoding="utf-8"?>
<abapGit version="v1.0.0" serializer="LCL_OBJECT_DTEL" serializer_version="v1.0.0">
 <asx:abap xmlns:asx="http://www.sap.com/abapxml" version="1.0">
  <asx:values>
   <DD04V>
    <ROLLNAME>AMOO</ROLLNAME>
    <DDLANGUAGE>E</DDLANGUAGE>
    <DOMNAME>FLAG</DOMNAME>
    <HEADLEN>10</HEADLEN>
    <SCRLEN1>10</SCRLEN1>
    <SCRLEN2>10</SCRLEN2>
    <SCRLEN3>20</SCRLEN3>
    <DDTEXT>MOO</DDTEXT>
    <REPTEXT>MOO</REPTEXT>
    <SCRTEXT_S>MOO</SCRTEXT_S>
    <SCRTEXT_M>MOO</SCRTEXT_M>
    <SCRTEXT_L>MOO</SCRTEXT_L>
    <DTELMASTER>D</DTELMASTER>
    <REFKIND>D</REFKIND>
   </DD04V>
  </asx:values>
 </asx:abap>
</abapGit>`;
    const file1 = {filename: "amoo.dtel.xml", contents: amoo_dtel};

    const flag_doma = `<?xml version="1.0" encoding="utf-8"?>
<abapGit version="v1.0.0" serializer="LCL_OBJECT_DOMA" serializer_version="v1.0.0">
 <asx:abap xmlns:asx="http://www.sap.com/abapxml" version="1.0">
  <asx:values>
   <DD01V>
    <DOMNAME>FLAG</DOMNAME>
    <DDLANGUAGE>E</DDLANGUAGE>
    <DATATYPE>CHAR</DATATYPE>
    <LENG>000001</LENG>
    <OUTPUTLEN>000001</OUTPUTLEN>
    <DDTEXT>FOO</DDTEXT>
   </DD01V>
  </asx:values>
 </asx:abap>
</abapGit>`;
    const file2 = {filename: "flag.doma.xml", contents: flag_doma};

    const char1_dtel = `<?xml version="1.0" encoding="utf-8"?>
<abapGit version="v1.0.0" serializer="LCL_OBJECT_DTEL" serializer_version="v1.0.0">
 <asx:abap xmlns:asx="http://www.sap.com/abapxml" version="1.0">
  <asx:values>
   <DD04V>
    <ROLLNAME>CHAR1</ROLLNAME>
    <DDLANGUAGE>E</DDLANGUAGE>
    <DOMNAME>FLAG</DOMNAME>
    <DDTEXT>CHAR1</DDTEXT>
    <DTELMASTER>D</DTELMASTER>
    <REFKIND>D</REFKIND>
   </DD04V>
  </asx:values>
 </asx:abap>
</abapGit>`;
    const file3 = {filename: "char1.dtel.xml", contents: char1_dtel};

    const clas = `CLASS zcl_foobar DEFINITION PUBLIC FINAL CREATE PUBLIC.
  PUBLIC SECTION.
    DATA mv_compress TYPE char1.
ENDCLASS.

CLASS zcl_foobar IMPLEMENTATION.

ENDCLASS.`;
    const file4 = {filename: "zcl_foobar.clas.abap", contents: clas};

    const output = await runFiles([file1, file2, file3, file4]);

    expect(output.length).to.equal(3);
//    console.dir(output[2].chunk.getCode());
    expect(output[2].chunk.getCode()).to.not.contain("AMOO");
  });

});