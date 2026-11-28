import {expect} from "chai";
import * as abaplint from "@abaplint/core";
import {Transpiler} from "../src";

const abapgitXML = `<?xml version="1.0" encoding="utf-8"?>
<abapGit version="v1.0.0">
</abapGit>`;

const testFiles: {[filename: string]: string} = {
  "zexample.ddlx.asddlxs": `@Metadata.layer: #CORE
annotate view ZDDLS with
{
}`,
  "zbdef.bdef.asbdef": `managed implementation in class zbp_example unique;
define behavior for ZDDLS
{
}`,
  "zsrvd.srvd.srvdsrv": `define service ZSRVD {
  expose ZDDLS;
}`,
  "zdcls.dcls.asdcls": `@MappingRole: true
define role ZDCLS {
  grant select on ZDDLS;
}`,
  "zaplo.aplo.xml": abapgitXML,
  "zsajc.sajc.xml": abapgitXML,
  "zsajt.sajt.xml": abapgitXML,
  "zsco2.sco2.xml": abapgitXML,
  "zsia6.sia6.xml": abapgitXML,
  "zsrvb.srvb.xml": abapgitXML,
  "zsmbc.smbc.json": `{
  "formatVersion": "1",
  "header": {
    "description": "AFF Example Object for SMBC",
    "originalLanguage": "en"
  },
  "serviceConfiguration": {
    "serviceBinding": "/DMO/SB_CUSTOMER_MSSK_O4",
    "serviceName": "/DMO/SD_CUSTOMER_MSSK",
    "serviceVersion": "0001",
    "rootEntitySet": "Customer",
    "name": "Maintain customer",
    "configurationGroup": "/DMO/ADMIN_GROUP",
    "skipRootEntity": false
  },
  "appConfiguration": {
    "listReport": {
      "variantManagement": "none"
    },
    "objectPages": [
      {
        "entitySet": "Customer",
        "sectionLayout": "tabs"
      }
    ],
    "tableSettings": [
      {
        "entitySet": "Customer",
        "tableType": "gridTable",
        "selectionMode": "auto",
        "enableFullScreen": true
      }
    ]
  }
}`,
};

describe("object types without output", () => {

  for (const [filename, contents] of Object.entries(testFiles)) {
    it(filename + " is allowed and produces no output", async () => {
      const file = new abaplint.MemoryFile(filename, contents);
      const reg = new abaplint.Registry().addFile(file);

      const res = await new Transpiler().run(reg);

      expect(res.objects.length).to.equal(0);
      expect(res.initializationScript).to.not.include(filename.split(".")[1]);
    });
  }

});
