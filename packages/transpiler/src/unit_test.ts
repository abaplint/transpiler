/* eslint-disable max-len */
import * as abaplint from "@abaplint/core";
import {escapeNamespaceFilename} from "./initialization";

export type TestMethodList = {object: string, class: string, method: string}[];

export class UnitTest {

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
        ret += `  await import("./${escapeNamespaceFilename(obj.getName().toLowerCase())}.${obj.getType().toLowerCase()}.testclasses.mjs");\n`;
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
import {initializeABAP} from "./init.mjs";

async function run() {
  await initializeABAP();\n`;

    for (const st of this.getSortedTests(reg)) {
      ret += `// --------------------------------------------\n`;
      const lc = st.localClass.toLowerCase();
      ret += `    {
        const {${lc}} = await import("./${escapeNamespaceFilename(st.obj.getName().toLowerCase())}.${st.obj.getType().toLowerCase()}.testclasses.mjs");
        if (${lc}.class_setup) await ${lc}.class_setup();\n`;

      for (const m of st.methods) {
        const skipThis = (skip || []).some(a => a.object === st.obj.getName() && a.class === lc && a.method === m);
        if (skipThis) {
          ret += `        console.log('${st.obj.getName()}: running ${lc}->${m}, skipped');\n`;
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
        ret += `        await test.FRIENDS_ACCESS_INSTANCE.${m}();\n`;
        ret += callSpecial("teardown");
        ret += `      }\n`;
      }

      ret += `        if (${lc}.class_teardown) await ${lc}.class_teardown();\n`;
      ret += `    }\n`;
    }

    ret += `// -------------------END-------------------
}

run().then(() => {
  process.exit(0);
}).catch((err) => {
  console.log(err);
  process.exit(1);
});`;

    return ret;
  }

}