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
      ret += `  sqlite.push(\`${i}\`);\n`;
    }
    ret += `  const hdb = \`${dbSetup.schemas.hdb}\`;\n`;
    ret += `  const pg = [];\n`;
    for (const i of dbSetup.schemas.pg) {
      ret += `  pg.push(\`${i}\`);\n`;
    }
    ret += `  const snowflake = [];\n`;
    for (const i of dbSetup.schemas.snowflake) {
      ret += `  snowflake.push(\`${i}\`);\n`;
    }
    ret += `  const schemas = {sqlite, hdb, pg, snowflake};\n`;
    ret += `\n`;

    ret += `  const insert = [];\n`;
    for (const i of dbSetup.insert) {
      ret += `  insert.push(\`${i}\`);\n`;
    }
    ret += `\n`;

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

  public unitTestScriptOpen(reg: abaplint.IRegistry, _skip?: TestMethodList): string {
    let ret = `/* eslint-disable curly */
import fs from "fs";
import path from "path";
import {fileURLToPath} from "url";
import {initializeABAP} from "./init.mjs";

const __dirname = path.dirname(fileURLToPath(import.meta.url));

async function run() {
  await initializeABAP();
  let lt_input = new abap.types.Table(new abap.types.Structure({class_name: new abap.types.Character(30), testclass_name: new abap.types.Character(30), method_name: new abap.types.Character(30)}), {"withHeader":false,"type":"STANDARD","isUnique":false,"keyFields":[]});
  let ls_input = new abap.types.Structure({class_name: new abap.types.Character(30), testclass_name: new abap.types.Character(30), method_name: new abap.types.Character(30)});
  let ls_result = new abap.types.Structure({list: new abap.types.Table(new abap.types.Structure({class_name: new abap.types.Character(30), testclass_name: new abap.types.Character(30), method_name: new abap.types.Character(30), expected: new abap.types.String(), actual: new abap.types.String(), status: new abap.types.String(), runtime: new abap.types.Integer(), message: new abap.types.String(), js_location: new abap.types.String(), console: new abap.types.String()}), {"withHeader":false,"type":"STANDARD","isUnique":false,"keyFields":[]}), json: new abap.types.String()});
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

  private getSortedTests(reg: abaplint.IRegistry) {
    const tests: {
      obj: abaplint.IObject,
      localClass: string,
      riskLevel: abaplint.Info.RiskLevel | undefined,
      duration: abaplint.Info.Duration | undefined,
      methods: string[],
    }[] = [];

    for (const obj of reg.getObjects()) {
      if (reg.isDependency(obj) || !(obj instanceof abaplint.Objects.Class)) {
        continue;
      }

      const hasTestFile = obj.getFiles().some(f => { return f.getFilename().includes(".testclasses."); });
      if (hasTestFile === false) {
        continue;
      }

      for (const file of obj.getABAPFiles()) {
        for (const def of file.getInfo().listClassDefinitions()) {
          if (def.isForTesting === false
              || def.isGlobal === true
              || def.methods.length === 0
              || def.isAbstract === true) {
            // todo, fix, there might be global test methods
            continue;
          }

          const methods: string[] = [];
          for (const m of def.methods) {
            if (m.isForTesting === false) {
              continue;
            }
            methods.push(m.name);
          }

          tests.push({
            obj,
            localClass: def.name,
            riskLevel: def.riskLevel,
            duration: def.duration,
            methods: methods,
          });
        }
      }
    }

    const toNumber = (riskLevel: abaplint.Info.RiskLevel | undefined, duration: abaplint.Info.Duration | undefined): number => {
      let int = 0;
      switch (riskLevel) {
        case abaplint.Info.RiskLevel.harmless:
          int = 10;
          break;
        case abaplint.Info.RiskLevel.critical:
          int = 20;
          break;
        case abaplint.Info.RiskLevel.dangerous:
          int = 30;
          break;
        default:
          break;
      }

      switch (duration) {
        case abaplint.Info.Duration.short:
          int += 1;
          break;
        case abaplint.Info.Duration.medium:
          int += 2;
          break;
        case abaplint.Info.Duration.long:
          int += 3;
          break;
        default:
          break;
      }
      return int;
    };

    tests.sort((a, b) => {
      const ai = toNumber(a.riskLevel, a.duration);
      const bi = toNumber(b.riskLevel, b.duration);
      let ret = ai - bi;

      if (ret === 0) {
        // if risk and duration are equal, then sort by name
        if (a.obj.getName() < b.obj.getName()) {
          ret = -1;
        } else if (a.obj.getName() > b.obj.getName()) {
          ret = 1;
        }
      }

      return ret;
    });

    return tests;
  }

  public unitTestScript(reg: abaplint.IRegistry, skip?: TestMethodList): string {
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

    for (const st of this.getSortedTests(reg)) {
      ret += `// --------------------------------------------\n`;
      ret += `    clas = unit.addObject("${st.obj.getName()}");\n`;
      const lc = st.localClass.toLowerCase();
      ret += `    {
        const {${lc}} = await import("./${this.escapeNamespace(st.obj.getName().toLowerCase())}.${st.obj.getType().toLowerCase()}.testclasses.mjs");
        locl = clas.addTestClass("${lc}");
        if (${lc}.class_setup) await ${lc}.class_setup();\n`;

      for (const m of st.methods) {
        const skipThis = (skip || []).some(a => a.object === st.obj.getName() && a.class === lc && a.method === m);
        if (skipThis) {
          ret += `  console.log('${st.obj.getName()}: running ${lc}->${m}, skipped');\n`;
          ret += `  meth = locl.addMethod("${m}");\n`;
          ret += `  meth.skip();\n`;
          continue;
        }

        const callSpecial = (name: string) => {
          let ret = "";
          ret += `        if (test.${name}) await test.${name}();\n`;
          ret += `        if (test.FRIENDS_ACCESS_INSTANCE.${name}) await test.FRIENDS_ACCESS_INSTANCE.${name}();\n`;
          ret += `        if (test.FRIENDS_ACCESS_INSTANCE.SUPER && test.FRIENDS_ACCESS_INSTANCE.SUPER.${name}) await test.FRIENDS_ACCESS_INSTANCE.SUPER.${name}();\n`;
          return ret;
        };

        ret += `      {\n        const test = await (new ${lc}()).constructor_();\n`;
        ret += callSpecial("setup");
        ret += `        console.log("${st.obj.getName()}: running ${lc}->${m}");\n`;
        ret += `        meth = locl.addMethod("${m}");\n`;
        ret += `        await test.FRIENDS_ACCESS_INSTANCE.${m}();\n`;
        ret += `        meth.pass();\n`;
        ret += callSpecial("teardown");
        ret += `      }\n`;
      }

      ret += `      if (${lc}.class_teardown) await ${lc}.class_teardown();\n`;
      ret += `    }\n`;
    }

    ret += `// -------------------END-------------------
    fs.writeFileSync(__dirname + path.sep + "_output.xml", unit.xUnitXML());
  } catch (e) {
    if (meth) {
      meth.fail();
    }
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
          || obj instanceof abaplint.Objects.LockObject
          || obj instanceof abaplint.Objects.MessageClass
          || obj instanceof abaplint.Objects.MIMEObject
          || obj instanceof abaplint.Objects.Oauth2Profile
          || obj instanceof abaplint.Objects.WebMIME
          || obj instanceof abaplint.Objects.TypePool
          || obj instanceof abaplint.Objects.TableType) {
        list.push(imp(`${this.escapeNamespace(obj.getName().toLowerCase())}.${obj.getType().toLowerCase()}`));
      }
    }

    for (const obj of reg.getObjects()) {
      if (obj instanceof abaplint.Objects.FunctionGroup) {
        list.push(imp(`${this.escapeNamespace(obj.getName().toLowerCase())}.fugr`));
      } else if (obj instanceof abaplint.Objects.Class) {
        if (obj.getName().toUpperCase() !== "CL_ABAP_CHAR_UTILITIES"
            && this.hasClassConstructor(reg, obj)) {
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

  // class constructors might make early use of eg. constants from interfaces
  // sub classes will import() super classes and trigger a class constructor of the super
  private hasClassConstructor(reg: abaplint.IRegistry, clas: abaplint.Objects.Class): boolean {
    if (clas.getDefinition()?.getMethodDefinitions().getByName("CLASS_CONSTRUCTOR") !== undefined) {
      return true;
    }

    const sup = clas.getDefinition()?.getSuperClass();
    if (sup !== undefined) {
      const superClass = reg.getObject("CLAS", sup) as abaplint.Objects.Class | undefined;
      if (superClass) {
        return this.hasClassConstructor(reg, superClass);
      }
    }

    return false;
  }

}