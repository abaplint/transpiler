import {Nodes, MemoryFile, Registry} from "abaplint";
import {Validation} from "./validation";
import * as StatementTranspilers from "./statements";

function traverseStatement(node: Nodes.StatementNode): string {
  const list: any = StatementTranspilers;
  for (const key in list) {
    const transpiler = new list[key]();
    if (node.get().constructor.name + "Transpiler" === transpiler.constructor.name) {
      return transpiler.transpile(node);
    }
  }

  return "todo, statement: " + node.get().constructor.name;
}

export function run(code: string): string {
  const file = new MemoryFile("zfoobar.prog.abap", code);
  const reg = new Registry().addFile(file);

  const issues = Validation.run(reg);
  if (issues.length > 0) {
    console.dir(issues);
    throw new Error("errors found");
  }

  const abap = reg.getABAPObjects()[0].getABAPFiles()[0];

  const result = abap.getStatements().map(s => traverseStatement(s)).join("\n");

  return result;
}