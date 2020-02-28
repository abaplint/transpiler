import {Nodes, Statements, MemoryFile, Registry} from "abaplint";
import {Validation} from "./validation";
import {MoveTranspiler, DataTranspiler, ElseTranspiler} from "./statements";

function traverseStatement(node: Nodes.StatementNode): string {
  if (node.get() instanceof Statements.Data) {
    return new DataTranspiler().transpile(node);
  } else if (node.get() instanceof Statements.Move) {
    return new MoveTranspiler().transpile(node);
  } else if (node.get() instanceof Statements.Else) {
    return new ElseTranspiler().transpile(node);
  }
  return "todo";
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

  let result = "";
  for(const s of abap.getStatements()) {
    result = result + traverseStatement(s);
  }
  return result;
}