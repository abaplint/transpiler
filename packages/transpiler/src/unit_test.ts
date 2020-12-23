import * as abaplint from "@abaplint/core";

export type SkipSettings = {object: string, class: string, method: string}[];

export class UnitTest {

  public run(reg: abaplint.IRegistry, dbSetup: string, skip?: SkipSettings): string {
    let ret = `const fs = require("fs");
const path = require("path");
const runtime = require("@abaplint/runtime");
global.abap = new runtime.ABAP();
async function initDB() {
return global.abap.initDB(\`${dbSetup}\`);
}
${this.functionGroups(reg)}
initDB().then(() => {
const unit = new runtime.UnitTestResult();
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
locl = clas.addTestClass("${def.name}");\n`;

            if (def.methods.some(m => m.name.toUpperCase() === "CLASS_SETUP")) {
              ret += `${def.name}.class_setup();\n`;
            }

            const hasSetup = def.methods.some(m => m.name.toUpperCase() === "SETUP");
            const hasTeardown = def.methods.some(m => m.name.toUpperCase() === "TEARDOWN");

            for (const m of def.methods) {
              if (m.isForTesting === false) {
                continue;
              }
              ret += `{\n  const test = new ${def.name}();\n`;
              if (hasSetup === true) {
                ret += `  test.setup();\n`;
              }

              ret += `  console.log('${obj.getName()}: running ${def.name}->${m.name}');\n`;
              ret += `  meth = locl.addMethod("${m.name}");\n`;
              if ((skip || []).some(a => a.object === obj.getName() && a.class === def.name && a.method === m.name)) {
                ret += `  meth.skip();\n`;
              } else {
                ret += `  test.${m.name}();\n`;
                ret += `  meth.pass();\n`;
              }

              if (hasTeardown === true) {
                ret += `  test.teardown();\n`;
              }
              ret += `}\n`;
            }

            if (def.methods.some(m => m.name.toUpperCase() === "CLASS_TEARDOWN")) {
              ret += `${def.name}.class_teardown();\n`;
            }

            ret += `}\n`;
          }
        }
      }
    }

    ret += `console.log(abap.console.get());
fs.writeFileSync(__dirname + path.sep + "output.xml", unit.xUnitXML());
} catch (e) {
  if (meth) {
    meth.fail();
  }
  console.log(abap.console.get());
  fs.writeFileSync(__dirname + path.sep + "output.xml", unit.xUnitXML());
  throw e;
}
}).catch((err) => {
  console.log(err);
  process.exit(1);
});`;

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