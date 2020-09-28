import * as abaplint from "@abaplint/core";

export class UnitTest {

  // with lots of assumptions regarding setup
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
const ${def.name} = require("./${obj.getName().toLowerCase()}.${obj.getType().toLowerCase()}").${def.name};
const test = new ${def.name}();\n`;

            if (def.methods.some(m => m.name.toUpperCase() === "CLASS_SETUP")) {
              ret += `test.class_setup();\n`;
            }

            const hasSetup = def.methods.some(m => m.name.toUpperCase() === "SETUP");
            const hasTeardown = def.methods.some(m => m.name.toUpperCase() === "TEARDOWN");

            for (const m of def.methods) {
              if (m.isForTesting === false) {
                continue;
              }
              if (hasSetup === true) {
                ret += `test.setup();\n`;
              }

              ret += `test.${m.name}();\n`;

              if (hasTeardown === true) {
                ret += `test.teardown();\n`;
              }
            }

            if (def.methods.some(m => m.name.toUpperCase() === "CLASS_TEARDOWN")) {
              ret += `test.class_teardown();\n`;
            }

            ret += `}\n`;
          }
        }
      }
    }

    return ret;
  }

}