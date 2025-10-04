import {DatabaseSetupResult} from "./db/database_setup_result";
import * as abaplint from "@abaplint/core";
import {ITranspilerOptions} from "./types";
import {HandleFUGR} from "./handlers/handle_fugr";

export function escapeNamespaceFilename(filename: string): string {
// ES modules are resolved and cached as URLs. This means that special characters must be
// percent-encoded, such as # with %23 and ? with %3F.
    return filename.replace(/\//g, "%23");
}

export class Initialization {

  public script(reg: abaplint.IRegistry, dbSetup: DatabaseSetupResult, options: ITranspilerOptions | undefined, useImport?: boolean) {
      let ret = "";
      if (useImport === true) {
        ret = `/* eslint-disable import/newline-after-import */
import "./_top.mjs";\n`;
      } else {
        ret = `/* eslint-disable import/newline-after-import */
import runtime from "@abaplint/runtime";
globalThis.abap = new runtime.ABAP();\n`;
      }

      if (options?.setup?.filename === undefined || options?.setup?.filename === "") {
        ret += `// no setup logic specified in config\n`;
      } else {
        ret += `const setup = await import("${options?.setup?.filename}");\n`;
      }

      ret += `export async function initializeABAP() {\n`;
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

      if (options?.setup?.preFunction !== undefined) {
        ret += `  await setup.${options?.setup?.preFunction}(globalThis.abap, schemas, insert);\n`;
      }
      ret += `}\n\n`;
      ret += `await initializeABAP();\n\n`;

      ret += `${this.buildImports(reg, useImport, options)}`;

      if (options?.setup?.postFunction !== undefined) {
        ret += `\n\nawait setup.${options?.setup?.postFunction}(globalThis.abap, schemas, insert);\n`;
      }

      return ret;
    }

  private buildImports(reg: abaplint.IRegistry, useImport?: boolean, options?: ITranspilerOptions): string {
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
        list.push(imp(`${escapeNamespaceFilename(obj.getName().toLowerCase())}.${obj.getType().toLowerCase()}`));
      }
    }

    for (const obj of reg.getObjects()) {
      const name = imp(`${escapeNamespaceFilename(obj.getName().toLowerCase())}.${obj.getType().toLowerCase()}`);
      if (obj instanceof abaplint.Objects.Class
          && obj.getName().toUpperCase() !== "CL_ABAP_CHAR_UTILITIES"
          && this.hasClassConstructor(reg, obj)) {
        // this will not solve all problems with class constructors 100%, but probably good enough
        late.push(name);
      } else if (obj instanceof abaplint.Objects.Program && obj.isInclude() === true) {
        continue;
      } else if (obj instanceof abaplint.Objects.FunctionGroup && HandleFUGR.shouldSkip(obj, reg) === true) {
        continue;
      } else if (obj instanceof abaplint.Objects.Interface
          || obj instanceof abaplint.Objects.FunctionGroup
          || (options?.importProg === true && obj instanceof abaplint.Objects.Program)
          || obj instanceof abaplint.Objects.Class) {
        list.push(name);
      }
    }

    return [...list.sort(), ...late].join("\n");
  }

  // class constructors might make early use of eg. constants from interfaces
  // sub classes will import() super classes and trigger a class constructor of the super
  private hasClassConstructor(reg: abaplint.IRegistry, clas: abaplint.Objects.Class): boolean {
    /*
    if (clas.getDefinition()?.getMethodDefinitions().getByName("CLASS_CONSTRUCTOR") !== undefined) {
      return true;
    }
    */

    // also take local class definitions into account
    for (const file of clas.getABAPFiles()) {
      for (const cdef of file.getInfo().listClassDefinitions()) {
        if (cdef.methods.some((m) => m.name.toUpperCase() === "CLASS_CONSTRUCTOR")) {
          return true;
        }
      }
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