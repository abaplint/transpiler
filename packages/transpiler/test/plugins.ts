import {expect} from "chai";
import * as abaplint from "@abaplint/core";
import {Transpiler} from "../src";
import {ITranspilerOptions, ITranspilerPlugin, IOutputFile} from "../src/types";
import {Chunk} from "../src/chunk";

const tranXML = `<?xml version="1.0" encoding="utf-8"?>
<abapGit version="v1.0.0" serializer="LCL_OBJECT_TRAN" serializer_version="v1.0.0">
 <asx:abap xmlns:asx="http://www.sap.com/abapxml" version="1.0">
  <asx:values>
   <TSTC>
    <TCODE>ZTEST</TCODE>
    <PGMNA>ZPROGRAM</PGMNA>
    <DYPNO>1000</DYPNO>
   </TSTC>
  </asx:values>
 </asx:abap>
</abapGit>`;

class HandleTransaction implements ITranspilerPlugin {
  public objectTypes(): string[] {
    return ["TRAN"];
  }

  public handleObject(obj: abaplint.IObject, _reg: abaplint.IRegistry, _options: ITranspilerOptions): IOutputFile[] | undefined {
    if (obj.getType() !== "TRAN") {
      return undefined;
    }

    const filename = obj.getXMLFile()?.getFilename().replace(".xml", ".mjs").toLowerCase();
    if (filename === undefined) {
      return [];
    }

    const output: IOutputFile = {
      object: {
        name: obj.getName(),
        type: obj.getType(),
      },
      filename: filename,
      chunk: new Chunk().appendString(`// plugin handled ${obj.getName()}`),
      requires: [],
      exports: [],
    };

    return [output];
  }
}

describe("plugins", () => {

  it("plugin handles TRAN", async () => {
    const file = new abaplint.MemoryFile("ztest.tran.xml", tranXML);
    const reg = new abaplint.Registry().addFile(file).parse();

    const res = await new Transpiler({}, new HandleTransaction()).run(reg);

    expect(res.objects.length).to.equal(1);
    expect(res.objects[0].object.type).to.equal("TRAN");
    expect(res.objects[0].filename).to.equal("ztest.tran.mjs");
    expect(res.objects[0].chunk.getCode()).to.include("plugin handled ZTEST");
  });

  it("plugin output is imported in initialization script", async () => {
    const file = new abaplint.MemoryFile("ztest.tran.xml", tranXML);
    const reg = new abaplint.Registry().addFile(file).parse();

    const res = await new Transpiler({}, new HandleTransaction()).run(reg);

    expect(res.initializationScript).to.include(`ztest.tran.mjs`);
  });

  it("without plugin, TRAN gives no output", async () => {
    const file = new abaplint.MemoryFile("ztest.tran.xml", tranXML);
    const reg = new abaplint.Registry().addFile(file).parse();

    const res = await new Transpiler().run(reg);

    expect(res.objects.length).to.equal(0);
    expect(res.initializationScript).to.not.include(`ztest.tran.mjs`);
  });

});
