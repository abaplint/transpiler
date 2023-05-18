import {Transpiler} from "../packages/transpiler/src/";
import {ABAP} from "../packages/runtime/src/";
import {SQLiteDatabaseClient} from "../packages/database-sqlite/src/";
import {PostgresDatabaseClient} from "../packages/database-pg/src";
import * as abaplint from "@abaplint/core";
import * as crypto from "node:crypto";
import {IFile, ITranspilerOptions} from "../packages/transpiler/src/types";

// see https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/AsyncFunction
// eslint-disable-next-line @typescript-eslint/no-empty-function
export const AsyncFunction = Object.getPrototypeOf(async ()=> {}).constructor;

export async function runFiles(abap: ABAP, files: IFile[]) {
  const memory = files.map(f => new abaplint.MemoryFile(f.filename, f.contents));
  const reg: abaplint.IRegistry = new abaplint.Registry().addFiles(memory).parse();
  const res = await new Transpiler().run(reg);
  abap.console.clear();
  if (res.databaseSetup.schemas.sqlite.length > 0) {
    abap.context.databaseConnections["DEFAULT"] = new SQLiteDatabaseClient();
    await abap.context.databaseConnections["DEFAULT"].connect();
    await abap.context.databaseConnections["DEFAULT"].execute(res.databaseSetup.schemas.sqlite);
    await abap.context.databaseConnections["DEFAULT"].execute(res.databaseSetup.insert);
  }
  let pre = "";
  for (const o of res.objects) {
    if (o.object.type === "TABL") {
      pre = o.chunk.getCode() + "\n";
    }
  }
  return "global.abap = abap;\n" + pre + res.objects[0].chunk.getCode();
}

export async function runFilesPostgres(abap: ABAP, files: IFile[]) {
  const memory = files.map(f => new abaplint.MemoryFile(f.filename, f.contents));
  const reg: abaplint.IRegistry = new abaplint.Registry().addFiles(memory).parse();
  const res = await new Transpiler().run(reg);
  abap.console.clear();
  if (res.databaseSetup.schemas.pg.length > 0) {
    const dbName = "transpiler-" + crypto.randomBytes(10).toString("hex");
    abap.context.databaseConnections["DEFAULT"] = new PostgresDatabaseClient({database: "postgres"});
    await abap.context.databaseConnections["DEFAULT"].connect();
    await abap.context.databaseConnections["DEFAULT"].execute(`CREATE DATABASE "${dbName}";`);
    await abap.context.databaseConnections["DEFAULT"].disconnect();

    abap.context.databaseConnections["DEFAULT"] = new PostgresDatabaseClient({database: dbName});
    await abap.context.databaseConnections["DEFAULT"].connect();
    await abap.context.databaseConnections["DEFAULT"].execute(res.databaseSetup.schemas.pg);
    await abap.context.databaseConnections["DEFAULT"].execute(res.databaseSetup.insert);
  }
  let pre = "";
  for (const o of res.objects) {
    if (o.object.type === "TABL") {
      pre = o.chunk.getCode() + "\n";
    }
  }
  return "global.abap = abap;\n" + pre + res.objects[0].chunk.getCode();
}

export async function compileFiles(files: IFile[], options?: ITranspilerOptions) {
  const memory = files.map(f => new abaplint.MemoryFile(f.filename, f.contents));
  const reg: abaplint.IRegistry = new abaplint.Registry().addFiles(memory).parse();
  const res = await new Transpiler(options).run(reg);
  return res;
}