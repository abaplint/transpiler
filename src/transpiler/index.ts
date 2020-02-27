import {Expressions, Nodes, Statements, MemoryFile, Registry} from "abaplint";
import {translateSource, translateTarget} from "./expressions";

function traverseStatement(node: Nodes.StatementNode): string {
  if (node.get() instanceof Statements.Data) {
    const name = node.findFirstExpression(Expressions.NamespaceSimpleName)!.getFirstToken().getStr();
    const type = node.findFirstExpression(Expressions.TypeName)!.getFirstToken().getStr();
    return "let " + name + " = new abap.basictypes." + type + "();";
  } else if (node.get() instanceof Statements.Move) {
    const source = translateSource(node.findDirectExpression(Expressions.Source)!);
    const target = translateTarget(node.findDirectExpression(Expressions.Target)!);
    return target + ".set(" + source + ");";
  } else if (node.get() instanceof Statements.Else) {
    return "} else {";
  }
  return "todo";
}

export function run(code: string): string {
  const file = new MemoryFile("zfoobar.prog.abap", code);
  const reg = new Registry().addFile(file).parse();
  const abap = reg.getABAPObjects()[0].getABAPFiles()[0];

  let result = "";
  for(const s of abap.getStatements()) {
    result = result + traverseStatement(s);
  }
  return result;
}