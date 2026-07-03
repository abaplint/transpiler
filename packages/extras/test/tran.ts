import {expect} from "chai";
import * as abaplint from "@abaplint/core";
import {Transpiler} from "@abaplint/transpiler";
import {plugin} from "../src";

const tranXML = `<?xml version="1.0" encoding="utf-8"?>
<abapGit version="v1.0.0" serializer="LCL_OBJECT_TRAN" serializer_version="v1.0.0">
 <asx:abap xmlns:asx="http://www.sap.com/abapxml" version="1.0">
  <asx:values>
   <TSTC>
    <TCODE>ZTEST</TCODE>
    <PGMNA>ZPROGRAM</PGMNA>
    <DYPNO>1000</DYPNO>
   </TSTC>
   <TSTCT>
    <SPRSL>E</SPRSL>
    <TCODE>ZTEST</TCODE>
    <TTEXT>Test transaction</TTEXT>
   </TSTCT>
  </asx:values>
 </asx:abap>
</abapGit>`;

describe("handle TRAN", () => {

  it("registers the transaction", async () => {
    const file = new abaplint.MemoryFile("ztest.tran.xml", tranXML);
    const reg = new abaplint.Registry().addFile(file).parse();

    const res = await new Transpiler({}, plugin).run(reg);

    expect(res.objects.length).to.equal(1);
    expect(res.objects[0].filename).to.equal("ztest.tran.mjs");
    const code = res.objects[0].chunk.getCode();
    expect(code).to.include(`abap.TRAN["ZTEST"]`);
    expect(code).to.include(`"program": "ZPROGRAM"`);
    expect(res.initializationScript).to.include("ztest.tran.mjs");
  });

});
