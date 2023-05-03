import {AsyncFunction, runFiles} from "../test/_utils.js";
import {ABAP} from "../packages/runtime/src/index.js";
import {performance} from "perf_hooks";
import * as fs from "fs";
import * as path from "path";
import * as url from "url";
import {test1} from "./test1.js";
import {test2} from "./test2.js";
import {test3} from "./test3.js";
import {test4} from "./test4.js";
import {test5} from "./test5.js";
import {test6} from "./test6.js";
import {test7} from "./test7.js";
import {test8} from "./test8.js";
import {test9} from "./test9.js";
import {test10} from "./test10.js";
import {test11} from "./test11.js";
import {test12} from "./test12.js";
import {test13} from "./test13.js";
import {test14} from "./test14.js";
import {test15} from "./test15.js";

// NOTE: does not run via Mocha

const __dirname = url.fileURLToPath(new URL(".", import.meta.url));

const abap: ABAP = new ABAP();

async function run(contents: string) {
  return runFiles(abap, [{filename: "zfoobar.prog.abap", contents}]);
}

type Test = {name: string, abap: string};
type Tests = Test[];
type Result = {name: string, runtime: number};
type Results = Result[];

const tests: Tests = [
  {name: "1: APPEND and DELETE", abap: test1},
  {name: "2: READ TABLE, table_line", abap: test2},
  {name: "3: LOOP USING KEY", abap: test3},
  {name: "4: Copy, same sorting", abap: test4},
  {name: "5: Copy, becomes sorted", abap: test5},
  {name: "6: INSERT INDEX 1", abap: test6},
  {name: "7: APPEND matching types", abap: test7},
  {name: "8: READ TABLE BINARY SEARCH", abap: test8},
  {name: "9: INSERT INTO TABLE hashed", abap: test9},
  {name: "10: DELETE ADJACENT", abap: test10},
  {name: "11: READ WITH TABLE KEY HASHED", abap: test11},
  {name: "12: READ WITH KEY HASHED, primary", abap: test12},
  {name: "13: INSERT INTO TABLE standard", abap: test13},
  {name: "14: Constant characters", abap: test14},
  {name: "15: Compare characters", abap: test15},
];

async function execute(t: Test) {
  const js = await run(t.abap);
  const f = new AsyncFunction("abap", js);

  const t0 = performance.now();
  await f(abap);
  const t1 = performance.now();

  return {name: t.name, runtime: Math.round(t1 - t0)};
}

async function start() {
  const results: Results = [];
  const time = new Date().toLocaleTimeString([], {hour: "2-digit", minute: "2-digit", hour12: false}) + " (" + Intl.DateTimeFormat().resolvedOptions().timeZone + ")";
  console.log("START RUNTIME PERFORMANCE TEST, " + time);
  for (const t of tests) {
    // @ts-ignore
    global.gc();
    const result = await execute(t);
    results.push(result);
    console.log(`${ result.name.padEnd(50, " ") } ${ ( result.runtime + "").padStart(4, " ") }ms`);
  }
  fs.writeFileSync(__dirname + path.sep + "results.json", JSON.stringify(results, null, 2));
}

start().then(() => {
  process.exit();
}).catch((err) => {
  console.log(err);
  process.exit(1);
});