import * as abaplint from "abaplint";

function traverseStatement(node: abaplint.Nodes.StatementNode): string {
  if (node.get() instanceof abaplint.Statements.Data) {
    const name = node.findFirstExpression(abaplint.Expressions.NamespaceSimpleName)!.getFirstToken().getStr();
    const type = node.findFirstExpression(abaplint.Expressions.TypeName)!.getFirstToken().getStr()
    return "let " + name + " = new abap.basictypes." + type + "();";
  } else if (node.get() instanceof abaplint.Statements.Move) {
// this only works for the single example
    const name = node.findFirstExpression(abaplint.Expressions.TargetField)!.getFirstToken().getStr();
    const int = node.findFirstExpression(abaplint.Expressions.Integer)!.getFirstToken().getStr();
    return name + ".set(" + int + ");";
  }
  return "todo";
}

function run(code: string): string {
  const file = new abaplint.MemoryFile("zfoobar.prog.abap", code);
  const reg = new abaplint.Registry().addFile(file).parse();
  const abap = reg.getABAPObjects()[0].getABAPFiles()[0];

  let result = "";
  for(const s of abap.getStatements()) {
    result = result + traverseStatement(s);
  }
  return result;
}

console.log(run("DATA foo TYPE i."));
console.log(run("foo = 2."));