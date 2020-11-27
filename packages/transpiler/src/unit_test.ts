import * as abaplint from "@abaplint/core";

export class UnitTest {

  public run(reg: abaplint.IRegistry): string {
    let ret = `const fs = require("fs");
const path = require("path");
global.abap = require("@abaplint/runtime");
${this.functionGroups(reg)}
const unit = new global.abap.UnitTestResult();
let clas;
let locl;
let meth;
try {\n`;

    for (const obj of reg.getObjects()) {
      if (obj instanceof abaplint.Objects.Class) {
        ret += `clas = unit.addObject("${obj.getName()}");\n`;
        for (const file of obj.getABAPFiles()) {
          for (const def of file.getInfo().listClassDefinitions()) {
            if (def.isForTesting === false) {
              continue;
            }
            ret += `{
const ${def.name} = require("./${obj.getName().toLowerCase()}.${obj.getType().toLowerCase()}.testclasses.js").${def.name};
locl = clas.addTestClass("${def.name}");
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
              ret += `{\n`;
              if (hasSetup === true) {
                ret += `  test.setup();\n`;
              }

              ret += `  console.log('${obj.getName()}: running ${def.name}->${m.name}');\n`;
              ret += `  meth = locl.addMethod("${m.name}");\n`;
              ret += `  test.${m.name}();\n`;
              ret += `  meth.pass();\n`;

              if (hasTeardown === true) {
                ret += `  test.teardown();\n`;
              }
              ret += `}\n`;
            }

            if (def.methods.some(m => m.name.toUpperCase() === "CLASS_TEARDOWN")) {
              ret += `test.class_teardown();\n`;
            }

            ret += `}\n`;
          }
        }
      }
    }

    ret += `console.log(abap.Console.get());
fs.writeFileSync(__dirname + path.sep + "output.xml", unit.xUnitXML());
} catch (e) {
  if (meth) {
    meth.fail();
  }
  console.log(abap.Console.get());
  fs.writeFileSync(__dirname + path.sep + "output.xml", unit.xUnitXML());
  throw e;
}`;

    return ret;
  }

  private functionGroups(reg: abaplint.IRegistry): string {
    let ret = "";
    for (const obj of reg.getObjects()) {
      if (obj instanceof abaplint.Objects.FunctionGroup) {
        for (const m of obj.getModules()) {
          ret += `require("./${obj.getName().toLowerCase()}.fugr.${m.getName().toLowerCase()}.js");\n`;
        }
      }
    }
    return ret;
  }

}