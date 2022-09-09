'use strict';
const fs = require("fs");
const crypto = require("crypto");
const childProcess = require("child_process");

const repos = [
  {name: "open-abap/open-abap",              success: undefined, command: "npm test"},
  {name: "open-abap/open-table-maintenance", success: undefined, command: "npm test"},
  {name: "abap-openapi/abap-openapi",        success: undefined, command: "npm test"},
  {name: "abapGit/abapGit",                  success: undefined, command: "npm run unit"},
  {name: "larshp/abap-advent-2020",          success: undefined, command: "npm test"},
  {name: "larshp/abapNTLM",                  success: undefined, command: "npm test"},
  {name: "larshp/abap-wasm",                 success: undefined, command: "npm test"},
//  {name: "SAP/abap-file-formats-tools",       command: "npm test"},
];

const CWD = "./.github/regression/";

for (const repo of repos) {
  repo.folderName = "regression-" + crypto.randomBytes(4).toString("hex");
  console.dir("Old Version: " + repo.name + ", " + repo.folderName);
  childProcess.execSync("git clone --depth=1 --recurse-submodules https://github.com/" + repo.name + ".git " + repo.folderName, {stdio: "inherit", cwd: CWD});
  childProcess.execSync("npm install", {stdio: "inherit", cwd: CWD + repo.folderName}); // install the transpiler version from NPM or fixed in the repo
  childProcess.execSync(repo.command, {stdio: "inherit", cwd: CWD + repo.folderName});
}

console.log("START NEW START NEW START NEW START NEW START NEW START NEW START NEW");

// compile local/new version
childProcess.execSync("npm run install", {stdio: "inherit"});
childProcess.execSync("npm run link-local", {stdio: "inherit"});
childProcess.execSync("npm run compile", {stdio: "inherit"});
childProcess.execSync("npm run link-local", {stdio: "inherit"});

for (let index = 0; index < repos.length; index++) {
  console.dir("New Version: " + repos[index].name + ", " + repos[index].folderName);
  childProcess.execSync("npm link @abaplint/transpiler-cli", {stdio: "inherit", cwd: CWD + repos[index].folderName});
  childProcess.execSync("npm link @abaplint/runtime", {stdio: "inherit", cwd: CWD + repos[index].folderName});
  try {
    childProcess.execSync(repos[index].command, {stdio: "inherit", cwd: CWD + repos[index].folderName});
    repos[index].success = true;
  } catch (e) {
    console.log("ERROR ERROR ERROR");
    console.dir(e);
    repos[index].success = false;
  }
}

// ============================

let comment = "Regression test results:\n";

comment += "| Repository | Result |\n";
comment += "| :--- | :---: |\n";
for (const repo of repos) {
  const link = "[" + repo.name + "](https://github.com/" + repo.name + ")"
  if (repo.success === true) {
    comment += "| " + link + " | :green_circle: |\n";
  } else {
    comment += "| " + link + " | :red_circle: |\n";
  }
}
comment += "\n";

console.dir(comment);

fs.writeFileSync("comment-regression.txt", comment);