import {rmSync} from "node:fs";
import {spawn, spawnSync} from "node:child_process";

const npx = process.platform === "win32" ? "npx.cmd" : "npx";

function run(command, args) {
  const result = spawnSync(command, args, {
    shell: process.platform === "win32",
    stdio: "inherit",
  });
  if (result.error) {
    throw result.error;
  }
  if (result.status !== 0) {
    process.exit(result.status ?? 1);
  }
}

async function startMock() {
  const server = spawn(process.execPath, ["test/mock.mjs"], {
    stdio: "inherit",
  });

  for (let i = 0; i < 50; i++) {
    if (server.exitCode !== null) {
      throw new Error(`Mock server exited early with code ${server.exitCode}`);
    }

    try {
      await fetch("http://localhost:45000");
      return server;
    } catch {
      await new Promise(resolve => setTimeout(resolve, 100));
    }
  }

  server.kill();
  throw new Error("Timed out waiting for mock server");
}

rmSync("output", {recursive: true, force: true});
run(npx, ["tsc"]);
run(npx, ["abap_transpile", "test/abap_transpile.json"]);

const server = await startMock();
try {
  run(process.execPath, ["output/index.mjs"]);
} finally {
  server.kill();
}
