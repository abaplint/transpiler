import * as abaplint from "@abaplint/core";

export type SkipSettings = {object: string, class: string, method: string}[];

export class UnitTest {

  public initializationScript(reg: abaplint.IRegistry, dbSetup: string) {
    return `import runtime from "@abaplint/runtime";
global.abap = new runtime.ABAP();
${this.buildImports(reg)}
export async function initDB() {
  return global.abap.initDB(\`${dbSetup}\`);
}`;
  }

  public unitTestScript(reg: abaplint.IRegistry, skip?: SkipSettings): string {
    let ret = `import fs from "fs";
import path from "path";
import {dirname} from 'path';
import {fileURLToPath} from 'url';
const __dirname = dirname(fileURLToPath(import.meta.url));
import {initDB} from "./init.mjs";
import runtime from "@abaplint/runtime";

async function run() {
  await initDB();
  const unit = new runtime.UnitTestResult();
  let clas;
  let locl;
  let meth;
try {\n`;

    for (const obj of reg.getObjects()) {
      if (reg.isDependency(obj) || !(obj instanceof abaplint.Objects.Class)) {
        continue;
      }
      ret += `// --------------------------------------------\n`;
      ret += `clas = unit.addObject("${obj.getName()}");\n`;
      for (const file of obj.getABAPFiles()) {
        for (const def of file.getInfo().listClassDefinitions()) {
          if (def.isForTesting === false
              || def.isGlobal === true // todo, fix, there might be global test methods
              || def.methods.length === 0) {
            continue;
          }
          ret += `{
const {${def.name}} = await import("./${obj.getName().toLowerCase()}.${obj.getType().toLowerCase()}.testclasses.mjs");
locl = clas.addTestClass("${def.name}");\n`;

          ret += `if (${def.name}.class_setup) await ${def.name}.class_setup();\n`;

          for (const m of def.methods) {
            if (m.isForTesting === false) {
              continue;
            }
            const skipThis = (skip || []).some(a => a.object === obj.getName() && a.class === def.name && a.method === m.name);
            if (skipThis) {
              ret += `  console.log('${obj.getName()}: running ${def.name}->${m.name}, skipped');\n`;
              ret += `  meth = locl.addMethod("${m.name}");\n`;
              ret += `  meth.skip();\n`;
              continue;
            }

            ret += `{\n  const test = await (new ${def.name}()).constructor_();\n`;
            ret += `  if (test.setup) await test.setup();\n`;
            ret += `  console.log('${obj.getName()}: running ${def.name}->${m.name}');\n`;
            ret += `  meth = locl.addMethod("${m.name}");\n`;
            ret += `  await test.${m.name}();\n`;
            ret += `  meth.pass();\n`;
            ret += `  if (test.teardown) await test.teardown();\n`;
            ret += `}\n`;
          }

          ret += `if (${def.name}.class_teardown) await ${def.name}.class_teardown();\n`;
          ret += `}\n`;
        }
      }
    }

    ret += `// -------------------END-------------------
console.log(abap.console.get());
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