'use strict';
const fs = require("fs");
const crypto = require("crypto");
const childProcess = require("child_process");

const repos = [
  {name: "abapGit/abapGit",                  command: "npm run unit"},
//  {name: "larshp/abap-advent-2020",          command: "npm test"},
//  {name: "larshp/abap-wasm",                 command: "npm test"},
  {name: "open-abap/open-abap",              command: "npm test"},
//  {name: "abap-openapi/abap-openapi-client", command: "npm test"},
//  {name: "larshp/abapNTLM",                  command: "npm test"},
];

const CWD = "./.github/regression/";

for (const repo of repos) {
  repo.folderName = "regression-" + crypto.randomBytes(4).toString("hex");
  console.dir("Old Version: " + repo.name + ", " + repo.folderName);
  childProcess.execSync("git clone --depth=1 --recurse-submodules https://github.com/" + repo.name + ".git " + repo.folderName, {stdio: "inherit", cwd: CWD});
  childProcess.execSync("npm install", {stdio: "inherit", cwd: CWD + repo.folderName}); // install the transpiler version from NPM or fixed in the repo
  childProcess.execSync(repo.command, {stdio: "inherit", cwd: CWD + repo.folderName});
}

// compile local/new version of the transpiler and runtime
childProcess.execSync("npm install", {stdio: "inherit"});
childProcess.execSync("npm run link-local", {stdio: "inherit"});
childProcess.execSync("npm test", {stdio: "inherit"});

for (const repo of repos) {
  console.dir("New Version: " + repo.name + ", " + repo.folderName);
  childProcess.execSync("npm link @abaplint/transpiler-cli", {stdio: "inherit", cwd: CWD + repo.folderName});
  childProcess.execSync("npm link @abaplint/runtime", {stdio: "inherit", cwd: CWD + repo.folderName});
  childProcess.execSync(repo.command, {stdio: "inherit", cwd: CWD + repo.folderName});
}

// ============================

let comment = "Regression test results:\n";

comment += "| Repo | Status |\n";
comment += "| :--- | :--- |\n";
for (const repo of repos) {
  comment += "| " + repo.name + " | :green_circle: |\n";
}
comment += "\n";

console.dir(comment);

fs.writeFileSync("comment-regression.txt", comment);