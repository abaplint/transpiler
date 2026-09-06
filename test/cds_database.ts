import {expect} from "chai";
import * as fs from "fs";
import * as os from "os";
import * as path from "path";
import {execFileSync} from "child_process";
import {pathToFileURL} from "url";
import initSqlJs from "sql.js";
import {Transpiler} from "../packages/transpiler/src";
import {plugin} from "../packages/extras/src";
import {viewFiles} from "../packages/extras/test/_cds";

async function checkRows(schemas: string[]) {
  const SQL = await initSqlJs();
  const db = new SQL.Database();
  try {
    schemas.forEach(sql => db.run(sql));
    db.run(`INSERT INTO t000 VALUES ('101', 'A'), ('102', 'B'), ('103', 'C'), ('999', 'Z');
      INSERT INTO t001 VALUES ('101', 'H'), ('102', 'J'), ('999', 'Z');
      INSERT INTO t002 VALUES ('101', 'E'), ('102', 'D'), ('999', 'E');
      INSERT INTO t003 VALUES ('101', 'X'), ('999', 'Y');`);
    expect(db.exec("SELECT * FROM zddls ORDER BY purchasingdocument")).to.deep.equal([{
      columns: ["purchasingdocument", "headercategory", "language", "changestatus"],
      values: [["101", "H", "E", "X"], ["102", "J", null, null]],
    }]);
  } finally {
    db.close();
  }
}

describe("CDS database integration", () => {
  it("queries the joined view using a registry from the caller's core installation", async () => {
    // Adapt the hook to avoid nominal Chunk types from two transpiler installs.
    const output = await new Transpiler({}, {
      objectTypes: () => ["DDLS"],
      handleObject: () => [],
      amendDatabaseSetup: plugin.amendDatabaseSetup?.bind(plugin),
    }).runRaw(viewFiles());
    await checkRows(output.databaseSetup.schemas.sqlite);
  });

  it("the bundled CLI writes and executes the same view in init.mjs", async function() {
    this.timeout(30000);
    const project = fs.mkdtempSync(path.join(os.tmpdir(), "transpiler-cds-"));
    try {
      fs.mkdirSync(path.join(project, "src"));
      for (const file of viewFiles()) {
        fs.writeFileSync(path.join(project, "src", file.filename), file.contents);
      }
      // Resolve the local plugin as an external package, exactly as consumers do.
      const moduleDir = path.join(project, "node_modules", "@abaplint", "transpiler-extras");
      fs.mkdirSync(moduleDir, {recursive: true});
      fs.writeFileSync(path.join(moduleDir, "index.js"),
        `module.exports = require(${JSON.stringify(require.resolve("../packages/extras/src"))});`);
      fs.writeFileSync(path.join(project, "package.json"), "{}");
      fs.writeFileSync(path.join(project, "abap_transpile.json"), JSON.stringify({
        input_folder: "src", output_folder: "output", libs: [],
        options: {setup: {filename: "../setup.mjs", preFunction: "setup"}},
      }));
      fs.writeFileSync(path.join(project, "setup.mjs"),
        "export function setup(_abap, schemas) { globalThis.cdsTestSchemas = schemas; }");
      const stdout = execFileSync(process.execPath, [path.resolve("packages/cli/build/bundle.js")], {
        cwd: project, encoding: "utf8", stdio: "pipe",
      });
      expect(stdout).to.include("Plugin loaded: @abaplint/transpiler-extras");
      const initialization = fs.readFileSync(path.join(project, "output/init.mjs"), "utf8");
      expect(initialization).to.include('CREATE VIEW "zddls"');
      // Execute the schema-building portion of the generated initializer. The
      // generated table runtime imports are unrelated to schema initialization.
      const script = initialization.slice(initialization.indexOf("export async function initializeABAP()"),
        initialization.indexOf("await initializeABAP();"));
      const schemasFile = path.join(project, "schemas.mjs");
      fs.writeFileSync(schemasFile, 'import * as setup from "./setup.mjs";\n' + script +
        "await initializeABAP();\nexport default globalThis.cdsTestSchemas;");
      // Preserve native dynamic import when TypeScript compiles to CommonJS.
      const schemas = await new Function("url", "return import(url)")(pathToFileURL(schemasFile).href);
      await checkRows(schemas.default.sqlite);
    } finally {
      delete (globalThis as any).cdsTestSchemas;
      fs.rmSync(project, {recursive: true, force: true});
    }
  });
});
