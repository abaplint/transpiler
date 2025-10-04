import {ITranspilerConfig} from "../packages/cli/src/types";
import {FileOperations} from "../packages/cli/src/file_operations";
import {compileFiles} from "test/_utils";

// only does building, does not execute runtime

// while in cwd = /, run with:
//   npm run compile && time node build/performance/adhoc.js
// assuming `npm install clinic -g`,
//   clinic flame -- node build/performance/adhoc.js

async function run() {
  const config: ITranspilerConfig = {
    input_folder: "./performance/adhoc",
    input_filter: [],
    output_folder: "",
    write_unit_tests: true,
    write_source_map: true,
    options: {
      "ignoreSyntaxCheck": false,
      "addFilenames": true,
      "addCommonJS": true,
    },
  };

  const files = await FileOperations.loadFiles(config);

  console.log("\nBuilding");
//  const t = new Transpiler.Transpiler(config.options);
  const output = await compileFiles(files, config.options as any);
  console.log(output.reg.getObjectCount() + " objects processed");
}

run().then(() => {
  process.exit();
}).catch((err) => {
  console.log(err);
  process.exit(1);
});