/* eslint-disable max-len */
import * as abaplint from "@abaplint/core";
import {DatabaseSetupResult} from "./db/database_setup_result";

export type TestMethodList = {object: string, class: string, method: string}[];

export class UnitTest {

  // todo, move this method somewhere else, its much more than just unit test relevant
  public initializationScript(reg: abaplint.IRegistry, dbSetup: DatabaseSetupResult, extraSetup?: string, useImport?: boolean) {
    let ret = "";
    if (useImport === true) {
      ret = `/* eslint-disable import/newline-after-import */
import "./_top.mjs";\n`;
    } else {
      ret = `/* eslint-disable import/newline-after-import */
import runtime from "@abaplint/runtime";
globalThis.abap = new runtime.ABAP();\n`;
    }

    ret += `${this.buildImports(reg, useImport)}

export async function initializeABAP() {\n`;
    ret += `  const sqlite = [];\n`;
    for (const i of dbSetup.schemas.sqlite) {
      ret += `sqlite.push(\`${i}\`);\n`;
    }
    ret += `  const hdb = \`${dbSetup.schemas.hdb}\`;\n`;
    ret += `  const pg = \`${dbSetup.schemas.pg}\`;\n`;
    ret += `  const schemas = {sqlite, hdb, pg};\n`;
    ret += `  const insert = [];\n`;
    for (const i of dbSetup.insert) {
      ret += `  insert.push(\`${i}\`);\n`;
    }
    if (extraSetup === undefined) {
      ret += `// no setup logic specified in config\n`;
    } else {
      ret += `  const {setup} = await import("${extraSetup}");\n` +
             `  await setup(globalThis.abap, schemas, insert);\n`;
    }
    ret += `}`;
    return ret;
  }

  private escapeNamespace(filename: string): string {
// ES modules are resolved and cached as URLs. This means that special characters must be percent-encoded, such as # with %23 and ? with %3F.
    return filename.replace(/\//g, "%23");
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
        ret += `  await import("./${this.escapeNamespace(obj.getName().toLowerCase())}.${obj.getType().toLowerCase()}.testclasses.mjs");\n`;
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
  abap.statements.append({source: ls_input, target: lt_input});\n`;
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
      const {${def.name}} = await import("./${this.escapeNamespace(obj.getName().toLowerCase())}.${obj.getType().toLowerCase()}.testclasses.mjs");
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
    fs.writeFileSync(__dirname + path.sep + "_output.xml", unit.xUnitXML());
  } catch (e) {
    if (meth) {
      meth.fail();
    }
    console.log(abap.console.get());
    fs.writeFileSync(__dirname + path.sep + "_output.xml", unit.xUnitXML());
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

  private buildImports(reg: abaplint.IRegistry, useImport?: boolean): string {
// note: ES modules are hoised, so use the dynamic import(), due to setting of globalThis.abap
// some sorting required: eg. a class constructor using constant from interface

    const list: string[] = [];
    const late: string[] = [];

    const imp = (filename: string) => {
      if (useImport === true) {
        return `import "./${filename}.mjs";`;
      } else {
        return `await import("./${filename}.mjs");`;
      }
    };

    for (const obj of reg.getObjects()) {
      if (obj instanceof abaplint.Objects.Table
          || obj instanceof abaplint.Objects.DataElement
          || obj instanceof abaplint.Objects.TableType) {
        list.push(imp(`${this.escapeNamespace(obj.getName().toLowerCase())}.${obj.getType().toLowerCase()}`));
      }
    }

    for (const obj of reg.getObjects()) {
      if (obj instanceof abaplint.Objects.FunctionGroup) {
        for (const m of obj.getModules()) {
          list.push(imp(`${this.escapeNamespace(obj.getName().toLowerCase())}.fugr.${m.getName().toLowerCase()}`));
        }
      } else if (obj instanceof abaplint.Objects.Class) {
        if (obj.getName().toUpperCase() !== "CL_ABAP_CHAR_UTILITIES"
            && obj.getDefinition()?.getMethodDefinitions().getByName("CLASS_CONSTRUCTOR") !== undefined) {
          // this will not solve all problems with class constors 100%, but probably good enough
          late.push(imp(`${this.escapeNamespace(obj.getName().toLowerCase())}.${obj.getType().toLowerCase()}`));
        } else {
          list.push(imp(`${this.escapeNamespace(obj.getName().toLowerCase())}.${obj.getType().toLowerCase()}`));
        }
      } else if (obj instanceof abaplint.Objects.Interface) {
        list.push(imp(`${this.escapeNamespace(obj.getName().toLowerCase())}.${obj.getType().toLowerCase()}`));
      }
    }

    return [...list.sort(), ...late].join("\n");
  }

}