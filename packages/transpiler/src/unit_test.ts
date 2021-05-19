import * as abaplint from "@abaplint/core";

export type SkipSettings = {object: string, class: string, method: string}[];

export class UnitTest {

  public run(reg: abaplint.IRegistry, dbSetup: string, skip?: SkipSettings): string {
    let ret = `import fs from "fs";
import path from "path";
import runtime from "@abaplint/runtime";
import {dirname} from 'path';
import {fileURLToPath} from 'url';
global.abap = new runtime.ABAP();
${this.buildImports(reg)}
const __dirname = dirname(fileURLToPath(import.meta.url));
async function initDB() {
  return global.abap.initDB(\`${dbSetup}\`);
}
async function run() {
await initDB();
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
const {${def.name}} = await import("./${obj.getName().toLowerCase()}.${obj.getType().toLowerCase()}.testclasses.mjs");
locl = clas.addTestClass("${def.name}");\n`;

            if (def.methods.some(m => m.name.toUpperCase() === "CLASS_SETUP")) {
              ret += `await ${def.name}.class_setup();\n`;
            }

            const hasSetup = def.methods.some(m => m.name.toUpperCase() === "SETUP");
            const hasTeardown = def.methods.some(m => m.name.toUpperCase() === "TEARDOWN");

            for (const m of def.methods) {
              if (m.isForTesting === false) {
                continue;
              }
              const skipThis = (skip || []).some(a => a.object === obj.getName() && a.class === def.name && a.method === m.name);
              if (skipThis === false) {
                ret += `{\n  const test = await (new ${def.name}()).constructor_();\n`;
                if (hasSetup === true) {
                  ret += `  await test.setup();\n`;
                }
              }

              const extraLog = skipThis ? ", skipped" : "";
              ret += `  console.log('${obj.getName()}: running ${def.name}->${m.name}${extraLog}');\n`;
              ret += `  meth = locl.addMethod("${m.name}");\n`;
              if (skipThis) {
                ret += `  meth.skip();\n`;
              } else {
                ret += `  await test.${m.name}();\n`;
                ret += `  meth.pass();\n`;
              }

              if (hasTeardown === true && skipThis === false) {
                ret += `  await test.teardown();\n`;
              }
              ret += `}\n`;
            }

            if (def.methods.some(m => m.name.toUpperCase() === "CLASS_TEARDOWN")) {
              ret += `await ${def.name}.class_teardown();\n`;
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
}

run().then(() => {
  process.exit();
}).catch((err) => {
  console.log(err);
  process.exit(1);
});`;

    return ret;
  }

  private buildImports(reg: abaplint.IRegistry): string {
// note: ES modules are hoised, so use the dynamic import()

// todo, some sorting might be required? eg. a class constructor using constant from interface?

// temporary sorting: by filename

    const list: string[] = [];

    for (const obj of reg.getObjects()) {
      if (obj instanceof abaplint.Objects.FunctionGroup) {
        for (const m of obj.getModules()) {
          list.push(`await import("./${obj.getName().toLowerCase()}.fugr.${m.getName().toLowerCase()}.mjs");`);
        }
      } else if (obj instanceof abaplint.Objects.Class) {
        list.push(`await import("./${obj.getName().toLowerCase()}.clas.mjs");`);
      } else if (obj instanceof abaplint.Objects.Interface) {
        list.push(`await import("./${obj.getName().toLowerCase()}.intf.mjs");`);
      }
    }

    list.sort();

    return list.join("\n") + "\n";
  }

}