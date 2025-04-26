'use strict';
const fs = require("fs");

const after = JSON.parse(fs.readFileSync("./results_after.json", "utf-8"));
const before = JSON.parse(fs.readFileSync("./results_before.json", "utf-8"));

const result = {};

for (const a of after) {
  if (result[a.name] === undefined) {
    result[a.name] = {};
  }
  result[a.name].after = a.runtime;
}

for (const b of before) {
  if (result[b.name] === undefined) {
    result[b.name] = {};
  }
  result[b.name].before = b.runtime;
}

console.dir(result);

let comment = "Performance test results:\n";

comment += "| Performance | Before | After | Delta |\n";
comment += "| :--- | ---: | ---: | ---: |\n";
for (const key of Object.keys(result)) {
  let delta = result[key].after - result[key].before;
  if (Math.abs(delta) > 200) {
    delta += " :red_circle:";
  } else {
    delta += " :green_circle:";
  }
  comment += "| " + key + " | " + result[key].before + "ms | " + result[key].after + "ms | " + delta + " |\n";
}

comment += "\nUpdated: " + new Date().toISOString() + "\n";
comment += "\nSHA: " + process.env.GITHUB_SHA + "\n";

console.dir(comment);

fs.writeFileSync("comment-performance.txt", comment);