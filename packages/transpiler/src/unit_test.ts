/* eslint-disable max-len */
import * as abaplint from "@abaplint/core";

export type TestMethodList = {object: string, class: string, method: string}[];

export class UnitTest {

  // todo, move this method somewhere else, its much more than just unit test relevant
  public initializationScript(reg: abaplint.IRegistry, dbSetup: string, extraSetup?: string) {
    let ret = `/* eslint-disable import/newline-after-import */
import runtime from "@abaplint/runtime";
global.abap = new runtime.ABAP();
${this.buildImports(reg)}

export async function initializeABAP() {\n`;
    ret += `  const SQLiteSchema = \`${dbSetup}\`;\n`;
    ret += `  const HDBSchema = \`\`;\n`;
    ret += `  const PGSchema = \`\`;\n`;
    ret += `  const schemas = {sqlite: SQLiteSchema, hdb: HDBSchema, pg: PGSchema};\n`;
    if (extraSetup === undefined) {
      ret += `// no setup logic specified in config\n`;
    } else {
      ret += `  const {setup} = await import("${extraSetup}");\n` +
             `  await setup(global.abap, schemas);\n`;
    }
    ret += `}`;
    return ret;
  }

  public unitTestScriptOpen(reg: abaplint.IRegistry, _skip?: TestMethodList, _only?: TestMethodList): string {
    let ret = `/* eslint-disable curly */
import fs from "fs";
import path from "path";
import {fileURLToPath} from "url";
import {initializeABAP} from "./init.mjs";

const __dirname = path.dirname(fileURLToPath(import.meta.url));

async function run() {
  await initializeABAP();
  let lt_input = new abap.types.Table(new abap.types.Structure({class_name: new abap.types.Character({length: 30}), testclass_name: new abap.types.Character({length: 30}), method_name: new abap.types.Character({length: 30})}), {"withHeader":false,"type":"STANDARD","isUnique":false,"keyFields":[]});
  let ls_input = new abap.types.Structure({class_name: new abap.types.Character({length: 30}), testclass_name: new abap.types.Character({length: 30}), method_name: new abap.types.Character({length: 30})});
  let ls_result = new abap.types.Structure({list: new abap.types.Table(new abap.types.Structure({class_name: new abap.types.Character({length: 30}), testclass_name: new abap.types.Character({length: 30}), method_name: new abap.types.Character({length: 30}), expected: new abap.types.String(), actual: new abap.types.String(), status: new abap.types.String(), runtime: new abap.types.Integer(), message: new abap.types.String(), js_location: new abap.types.String()}), {"withHeader":false,"type":"STANDARD","isUnique":false,"keyFields":[]}), json: new abap.types.String()});
`;

    for (const obj of reg.getObjects()) {
      if (reg.isDependency(obj) || !(obj instanceof abaplint.Objects.Class)) {
        continue;
      }
      const hasTestFile = obj.getFiles().some(f => { return f.getFilename().includes(".testclasses."); });
      if (hasTestFile === true) {
        ret += `  await import("./${obj.getName().toLowerCase()}.${obj.getType().toLowerCase()}.testclasses.mjs");\n`;
      }

      for (const file of obj.getABAPFiles()) {
        for (const def of file.getInfo().listClassDefinitions()) {
          if (def.isForTesting === false || def.isGlobal === true  || def.methods.length === 0) {
          // todo, fix, there might be global test methods
            continue;
          }

          for (const m of def.methods) {
            if (m.isForTesting === false) {
              continue;
            }
            ret += `  ls_input.get().class_name.set("${obj.getName()}");
  ls_input.get().testclass_name.set("${def.name.toUpperCase()}");
  ls_input.get().method_name.set("${m.name.toUpperCase()}");
  abap.statements.append({source: ls_input, target: lt_input});`;
          }
        }
      }
    }

    ret += `

  ls_result.set(await abap.Classes["KERNEL_UNIT_RUNNER"].run({it_input: lt_input}));
  fs.writeFileSync(__dirname + path.sep + "output.json", ls_result.get().json.get());
}

run().then(() => {
  process.exit(0);
}).catch((err) => {
  console.log(err);
  process.exit(1);
});`;

    return ret;
  }

  public unitTestScript(reg: abaplint.IRegistry, skip?: TestMethodList, _only?: TestMethodList): string {
    let ret = `/* eslint-disable curly */
import fs from "fs";
import path from "path";
import {fileURLToPath} from "url";
import {initializeABAP} from "./init.mjs";
import runtime from "@abaplint/runtime";

const __dirname = path.dirname(fileURLToPath(import.meta.url));

async function run() {
  await initializeABAP();
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
      ret += `    clas = unit.addObject("${obj.getName()}");\n`;
      for (const file of obj.getABAPFiles()) {
        for (const def of file.getInfo().listClassDefinitions()) {
          if (def.isForTesting === false || def.isGlobal === true || def.methods.length === 0) {
            // todo, fix, there might be global test methods
            continue;
          }
          ret += `    {
      const {${def.name}} = await import("./${obj.getName().toLowerCase()}.${obj.getType().toLowerCase()}.testclasses.mjs");
      locl = clas.addTestClass("${def.name}");
      if (${def.name}.class_setup) await ${def.name}.class_setup();\n`;

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

            ret += `      {\n        const test = await (new ${def.name}()).constructor_();\n`;
            ret += `        if (test.setup) await test.setup();\n`;
            ret += `        console.log("${obj.getName()}: running ${def.name}->${m.name}");\n`;
            ret += `        meth = locl.addMethod("${m.name}");\n`;
            ret += `        await test.${m.name}();\n`;
            ret += `        meth.pass();\n`;
            ret += `        if (test.teardown) await test.teardown();\n`;
            ret += `      }\n`;
          }

          ret += `      if (${def.name}.class_teardown) await ${def.name}.class_teardown();\n`;
          ret += `    }\n`;
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
  process.exit(0);
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
      if (obj instanceof abaplint.Objects.Table) {
        list.push(`await import("./${obj.getName().toLowerCase()}.tabl.mjs");`);
      } else if (obj instanceof abaplint.Objects.DataElement) {
        list.push(`await import("./${obj.getName().toLowerCase()}.dtel.mjs");`);
      } else if (obj instanceof abaplint.Objects.TableType) {
        list.push(`await import("./${obj.getName().toLowerCase()}.ttyp.mjs");`);
      }
    }

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

    return list.join("\n");
  }

}