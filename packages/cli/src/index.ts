import * as fs from "fs";
import * as path from "path";
import * as glob from "glob";
import * as childProcess from "child_process";
import * as os from "os";
import * as ProgressBar from "progress";
import * as Transpiler from "@abaplint/transpiler";
import {ITranspilerConfig, TranspilerConfig} from "./config";
import {FileOperations} from "./file_operations";

class Progress implements Transpiler.IProgress {
  private bar: ProgressBar;

  public set(total: number, _text: string) {
    this.bar = new ProgressBar(":percent - :elapseds - :text", {total, renderThrottle: 100});
  }

  public async tick(text: string) {
    this.bar.tick({text});
    this.bar.render();
  }
}

function loadLib(config: ITranspilerConfig): Transpiler.IFile[] {
  const files: Transpiler.IFile[] = [];
  if (config.lib && config.lib !== "") {
    console.log("Clone: " + config.lib);
    const dir = fs.mkdtempSync(path.join(os.tmpdir(), "abap_transpile-"));
    childProcess.execSync("git clone --quiet --depth 1 " + config.lib + " .", {cwd: dir, stdio: "inherit"});
    let count = 0;
    for (let filename of glob.sync(dir + "/src/**", {nosort: true, nodir: true})) {
      const contents = fs.readFileSync(filename, "utf8");
      filename = path.basename(filename);
      files.push({filename, contents});
      count++;
    }
    console.log(count + " files added from lib");
    FileOperations.deleteFolderRecursive(dir);
  }
  return files;
}

async function run() {
  console.log("Transpiler CLI");

    //in case we are requested to provide verbose output, save it to the attribute and remove the verbose components from parameters
  let verbose: boolean = false;
  if (process.argv.find(element => element === "-verbose") !== undefined) {
    verbose = true;
    const verbose_index = process.argv.indexOf("-verbose", 0);
    process.argv.splice(verbose_index, 1);
  }

  if (verbose) {
    console.log("\Running in verbose mode");
  }


  if (process.argv.find(element => element === "-default_config") !== undefined) {
    const defaultConfigPath = process.cwd() + path.sep + "abap_transpile.json";
    console.log("Creating default config using path: " + defaultConfigPath);
    fs.writeFileSync(defaultConfigPath, JSON.stringify(TranspilerConfig.getDefaultConfig()));
    console.log("default config created");
    return;
  }


  const config = TranspilerConfig.find(process.argv[2]);

  if (verbose) {
    console.log(config);
  }

  const files = FileOperations.loadFiles(config).concat(loadLib(config));

  if (verbose) {
    console.log("\nSource files");
    for (const file of files) {
      console.log("\n" + file.filename);
    }
  }

  console.log("\nBuilding");
  const t = new Transpiler.Transpiler(config.options);
  const output = await t.run(files, new Progress());

  console.log("\nOutput");
  const outputFolder = config.output_folder;
  if (!fs.existsSync(outputFolder)) {
    fs.mkdirSync(outputFolder);
  }

  for (const o of output.objects) {
    fs.writeFileSync(outputFolder + path.sep + o.js.filename, o.js.contents);
  }
  console.log(output.objects.length + " files written");

  if (config.write_unit_tests === true) {
    fs.writeFileSync(outputFolder + path.sep + "index.mjs", output.unitTest);
  }
}

run().then(() => {
  process.exit();
}).catch((err) => {
  console.log(err);
  process.exit(1);
});