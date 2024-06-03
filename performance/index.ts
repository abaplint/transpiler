import {AsyncFunction, runFiles} from "../test/_utils";
import {ABAP, MemoryConsole} from "../packages/runtime/src";
import {performance} from "perf_hooks";
import * as fs from "fs";
import * as path from "path";
import {test1} from "./test1";
import {test2} from "./test2";
import {test3} from "./test3";
import {test4} from "./test4";
import {test5} from "./test5";
import {test6} from "./test6";
import {test7} from "./test7";
import {test8} from "./test8";
import {test9} from "./test9";
import {test10} from "./test10";
import {test11} from "./test11";
import {test12} from "./test12";
import {test13} from "./test13";
import {test14} from "./test14";
import {test15} from "./test15";
import {test16} from "./test16";
import {test17} from "./test17";
import {test18} from "./test18";
import {test19} from "./test19";
import {test20} from "./test20";
import {test21} from "./test21";
import {test22} from "./test22";
import {test23} from "./test23";
import {test24} from "./test24";
import {test25} from "./test25";
import {test26} from "./test26";
import {test27} from "./test27";
import {test28} from "./test28";
import {test29} from "./test29";
import {test30} from "./test30";
import {test31} from "./test31";
import {test32} from "./test32";
import {test33} from "./test33";
import {test34} from "./test34";
import {test35} from "./test35";

// NOTE: does not run via Mocha

const abap: ABAP = new ABAP({console: new MemoryConsole()});

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
  {name: "16: Method number 5 :notes:", abap: test16},
  {name: "17: Substring and find negative", abap: test17},
  {name: "18: CO compare", abap: test18},
  {name: "19: Call method, compatible structure", abap: test19},
  {name: "20: Call method, identical structure", abap: test20},
  {name: "21: CASE many char constants", abap: test21},
  {name: "22: READ TABLE WITH KEY secondary", abap: test22},
  {name: "23: CASE many int constants", abap: test23},
  {name: "24: Copy table contents", abap: test24},
  {name: "25: READ TABLE, not found", abap: test25},
  {name: "26: REPLACE OCCURRENCES, simple", abap: test26},
  {name: "27: READ TABLE, building hash", abap: test27},
  {name: "28: CONCATENATE", abap: test28},
  {name: "29: Write Hex to Hex via offset", abap: test29},
  {name: "30: Get Hex from Hex via offset", abap: test30},
  {name: "31: SET BIT hex", abap: test31},
  {name: "32: GET BIT hex", abap: test32},
  {name: "33: Compare Hex", abap: test33},
  {name: "34: Basic CONCATENATE", abap: test34},
  {name: "35: gt", abap: test35},
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