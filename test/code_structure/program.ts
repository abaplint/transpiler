// import {expect} from "chai";
import {compileFiles} from "../_utils";

describe("Running code structure - Program", () => {

  it("a include in itself should be skipped", async () => {
    const code = `DATA foo TYPE i.`;
    const xml = `<?xml version="1.0" encoding="utf-8"?>
<abapGit version="v1.0.0" serializer="LCL_OBJECT_PROG" serializer_version="v1.0.0">
 <asx:abap xmlns:asx="http://www.sap.com/abapxml" version="1.0">
  <asx:values>
   <PROGDIR>
    <NAME>ZINCLUDE</NAME>
    <SUBC>I</SUBC>
    <RSTAT>K</RSTAT>
    <RLOAD>E</RLOAD>
   </PROGDIR>
  </asx:values>
 </asx:abap>
</abapGit>`;
    await compileFiles([
      {filename: "zinclude.prog.abap", contents: code},
      {filename: "zinclude.prog.xml", contents: xml},
    ]);
  });

});