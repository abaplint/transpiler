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
              ret += `{\n  const test = await (new ${def.name}()).constructor_();\n`;
              if (hasSetup === true) {
                ret += `  await test.setup();\n`;
              }

              ret += `  console.log('${obj.getName()}: running ${def.name}->${m.name}');\n`;
              ret += `  meth = locl.addMethod("${m.name}");\n`;
              if ((skip || []).some(a => a.object === obj.getName() && a.class === def.name && a.method === m.name)) {
                ret += `  meth.skip();\n`;
              } else {
                ret += `  await test.${m.name}();\n`;
                ret += `  meth.pass();\n`;
              }

              if (hasTeardown === true) {
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
// note: es modules are hoised, so use the dynamic import()

// todo, some sorting might be required? eg. a class constructor using constant from interface?

// temporary sorting:
// 1: dependency interfaces
// 2: dependency classes
// 3: interfaces
// 4: classes
// 5: function groups

    const list: {sort: number, code: string}[] = [];

    for (const obj of reg.getObjects()) {
      if (obj instanceof abaplint.Objects.FunctionGroup) {
        for (const m of obj.getModules()) {
          list.push({sort: 5, code: `await import("./${obj.getName().toLowerCase()}.fugr.${m.getName().toLowerCase()}.mjs");`});
        }
      } else if (obj instanceof abaplint.Objects.Class) {
        const sort = reg.isDependency(obj) ? 2 : 4;
        list.push({sort, code: `await import("./${obj.getName().toLowerCase()}.clas.mjs");`});
      } else if (obj instanceof abaplint.Objects.Interface) {
        const sort = reg.isDependency(obj) ? 1 : 3;
        list.push({sort, code: `await import("./${obj.getName().toLowerCase()}.intf.mjs");`});
      }
    }

    list.sort((a, b) => { return a.sort - b.sort; });

    return list.map(a => a.code).join("\n") + "\n";
  }

}