import {AsyncFunction, runFiles} from "../test/_utils";
import {ABAP} from "../packages/runtime/src";
import {performance} from "perf_hooks";
import * as fs from "fs";
import * as path from "path";
import {test1} from "./test1";
import {test2} from "./test2";

// NOTES
// * does not run via Mocha
// * Each test targeted to run around 1-2 seconds

const abap: ABAP = new ABAP();

async function run(contents: string) {
  return runFiles(abap, [{filename: "zfoobar.prog.abap", contents}]);
}

type Tests = {name: string, abap: string}[];
type Results = {name: string, runtime: number}[];

const tests: Tests = [
  {name: "Internal table, APPEND and DELETE", abap: test1},
  {name: "Internal table, READ TABLE, table_line", abap: test2},
];

async function execute() {
  const result: Results = [];
  for (const t of tests) {
    const js = await run(t.abap);
    const f = new AsyncFunction("abap", js);

    const t0 = performance.now();
    await f(abap);
    const t1 = performance.now();

    result.push({name: t.name, runtime: Math.round(t1 - t0)});
  }
  return result;
}

async function start() {
  const time = new Date().toLocaleTimeString([], {hour: "2-digit", minute: "2-digit", hour12: false}) + " (" + Intl.DateTimeFormat().resolvedOptions().timeZone + ")";
  console.log("START RUNTIME PERFORMANCE TEST, " + time);
  const results = await execute();
  let index = 1;
  for (const r of results) {
    console.log(`${ (index++ + "").padStart(3, "0") }: ${ r.name.padEnd(50, " ") } ${ r.runtime }ms`);
  }
  fs.writeFileSync(__dirname + path.sep + "results.json", JSON.stringify(results, null, 2));
}

start().then(() => {
  process.exit();
}).catch((err) => {
  console.log(err);
  process.exit(1);
});