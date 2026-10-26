/* eslint-disable max-len */
import * as abaplint from "@abaplint/core";
import {OutputLayout, importPath} from "./output_layout";

/** @uniqueItems true */
export type TestMethodList = {
  /** @pattern ^[a-zA-Z0-9_\/]+$ */
  object: string,
  /** @pattern ^[a-zA-Z0-9_\/]+$ */
  class: string,
  /** @pattern ^[a-zA-Z0-9_\/]+$ */
  method: string,
  note?: string,
}[];

export class UnitTest {

  public constructor(private readonly layout = new OutputLayout()) {
    // Test runners live at the output root.
  }

  public unitTestScriptOpen(reg: abaplint.IRegistry, _skip?: TestMethodList): string {
    let ret = `/* eslint-disable curly */
import fs from "fs";
import path from "path";
import {fileURLToPath} from "url";
import "./init.mjs";

const __dirname = path.dirname(fileURLToPath(import.meta.url));

async function run() {
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
        ret += `  await import("${importPath("index.mjs", this.layout.objectModule(obj, ".testclasses"))}");\n`;
      }

      for (const file of obj.getABAPFiles()) {
        for (const def of file.getInfo().listClassDefinitions()) {
          if (def.isForTesting === false || def.isGlobal === true || def.isAbstract === true) {
          // todo, fix, there might be global test methods
            continue;
          }

          for (const m of this.findTestMethods(def, obj, reg)) {
            ret += `  ls_input.get().class_name.set("${obj.getName()}");
  ls_input.get().testclass_name.set("${def.name.toUpperCase()}");
  ls_input.get().method_name.set("${m.toUpperCase()}");
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

  /** the FOR TESTING methods of the class, with those inherited from its superclasses:
   * a concrete test class may get all its tests from a local ABSTRACT base, the way a
   * suite runs the same tests against several implementations. A redefinition, or a
   * method of the same name, replaces the ancestor's entry */
  private findTestMethods(def: abaplint.Info.InfoClassDefinition, obj: abaplint.Objects.Class, reg: abaplint.IRegistry): string[] {
    const chain: abaplint.Info.InfoClassDefinition[] = [];
    const seen = new Set<string>();
    let current: abaplint.Info.InfoClassDefinition | undefined = def;
    while (current !== undefined && seen.has(current.name.toUpperCase()) === false) {
      seen.add(current.name.toUpperCase());
      chain.push(current);
      current = this.findClassDefinition(current.superClassName, obj, reg);
    }

    // the root ancestor's methods first, so a redefinition keeps its place in the order
    const methods = new Map<string, string>();
    for (const c of chain.reverse()) {
      for (const m of c.methods) {
        if (m.isForTesting === false) {
          continue;
        }
        methods.set(m.name.toUpperCase(), m.name);
      }
    }
    return [...methods.values()];
  }

  /** a local class of the same object, in any of its files, or else a global class */
  private findClassDefinition(name: string | undefined, obj: abaplint.Objects.Class, reg: abaplint.IRegistry): abaplint.Info.InfoClassDefinition | undefined {
    if (name === undefined) {
      return undefined;
    }
    const upper = name.toUpperCase();
    for (const file of obj.getABAPFiles()) {
      for (const def of file.getInfo().listClassDefinitions()) {
        if (def.name.toUpperCase() === upper) {
          return def;
        }
      }
    }
    const global = reg.getObject("CLAS", upper);
    if (global instanceof abaplint.Objects.Class) {
      for (const file of global.getABAPFiles()) {
        for (const def of file.getInfo().listClassDefinitions()) {
          if (def.name.toUpperCase() === upper) {
            return def;
          }
        }
      }
    }
    return undefined;
  }

  private getSortedTests(reg: abaplint.IRegistry) {
    const tests: {
      obj: abaplint.IObject,
      localClass: string,
      filename: string,
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
              || def.isAbstract === true) {
            // todo, fix, there might be global test methods
            continue;
          }

          const methods = this.findTestMethods(def, obj, reg);
          if (methods.length === 0) {
            // FOR TESTING helper class without test methods, nothing to run; it might also
            // live in locals_imp, which is not exported from the .testclasses module
            continue;
          }

          tests.push({
            obj,
            filename: `${importPath("index.mjs", this.layout.objectModule(obj, ".testclasses"))}`,
            localClass: def.name.toLowerCase(),
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
    const callSpecial = (name: string) => {
      let ret = "";
      ret += `if (test.${name}) await test.${name}();\n`;
      ret += `        if (test.FRIENDS_ACCESS_INSTANCE.${name}) await test.FRIENDS_ACCESS_INSTANCE.${name}();\n`;
      ret += `        if (test.FRIENDS_ACCESS_INSTANCE.SUPER && test.FRIENDS_ACCESS_INSTANCE.SUPER.${name}) await test.FRIENDS_ACCESS_INSTANCE.SUPER.${name}();`;
      return ret;
    };

    let ret = `/* eslint-disable curly */
/* eslint-disable max-len */
import "./init.mjs";

function getData() {
  const ret = [];\n`;
  for (const st of this.getSortedTests(reg)) {
    const methods = [];
    for (const m of st.methods) {
      const skipThis = (skip || []).some(a => a.object.toUpperCase() === st.obj.getName().toUpperCase()
        && a.class.toUpperCase() === st.localClass.toUpperCase()
        && a.method.toUpperCase() === m.toUpperCase());
      methods.push({
        name: m,
        skip: skipThis,
      });
    }

    ret += `  ret.push({objectName: "${st.obj.getName()}",
            localClass: "${st.localClass}",
            methods: ${JSON.stringify(methods)},
            riskLevel: "${st.riskLevel}",
            filename: "${st.filename}"});\n`;
  }
ret += `  return ret;
}

async function run() {
  const skipCritical = process.argv[2] === "--skip-critical";
  const onlyCritical = process.argv[2] === "--only-critical";
  for (const st of getData()) {
    const imported = await import(st.filename);
    const localClass = imported[st.localClass];
    if (localClass.class_setup) await localClass.class_setup();
    for (const m of st.methods) {
      const prefix = st.objectName + ": running " + st.localClass + "->" + m.name;
      if (m.skip) {
        console.log(prefix + ", skipped due to configuration");
      } else if (skipCritical && st.riskLevel === "CRITICAL") {
        console.log(prefix + ", skipped due to risk level " + st.riskLevel);
      } else if (onlyCritical && st.riskLevel !== "CRITICAL") {
        console.log(prefix + ", skipped due to risk level " + st.riskLevel);
      } else {
        const test = await (new localClass()).constructor_();
        ${callSpecial("setup")}
        console.log(prefix);
        await test.FRIENDS_ACCESS_INSTANCE[m.name]();
        ${callSpecial("teardown")}
      }
    }
    if (localClass.class_teardown) await localClass.class_teardown();
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

}
