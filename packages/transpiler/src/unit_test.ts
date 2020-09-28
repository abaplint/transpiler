import * as abaplint from "@abaplint/core";

export class UnitTest {

  // with lots of assumptions regarding setup
  // todo: SETUP and TEARDOWN methods
  public run(reg: abaplint.IRegistry): string {
    let ret = `global.abap = require("@abaplint/runtime");\n`;

    for (const obj of reg.getObjects()) {
      if (obj instanceof abaplint.ABAPObject) {
        for (const file of obj.getABAPFiles()) {
          for (const def of file.getInfo().listClassDefinitions()) {
            if (def.isForTesting === false) {
              continue;
            }
            ret += `{
const ${def.name} = require("./output/${obj.getName().toLowerCase()}.${obj.getType().toLowerCase}").${def.name};
const test = new ${def.name}();\n`;
            for (const m of def.methods) {
              if (m.isForTesting === false) {
                continue;
              }
              ret += `test.${m.name}();\n`;
            }
            ret += `}\n`;
          }
        }
      }
    }

    return ret;
  }

}