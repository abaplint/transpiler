import * as abaplint from "abaplint";

function translateSource(node: abaplint.Nodes.ExpressionNode): string {
  const int = node.findFirstExpression(abaplint.Expressions.Integer)!.getFirstToken().getStr();
  return int;
}

function translateTarget(node: abaplint.Nodes.ExpressionNode): string {
  const name = node.findFirstExpression(abaplint.Expressions.TargetField)!.getFirstToken().getStr();
  return name;
}

function traverseStatement(node: abaplint.Nodes.StatementNode): string {
  if (node.get() instanceof abaplint.Statements.Data) {
    const name = node.findFirstExpression(abaplint.Expressions.NamespaceSimpleName)!.getFirstToken().getStr();
    const type = node.findFirstExpression(abaplint.Expressions.TypeName)!.getFirstToken().getStr()
    return "let " + name + " = new abap.basictypes." + type + "();";
  } else if (node.get() instanceof abaplint.Statements.Move) {
    const source = translateSource(node.findDirectExpression(abaplint.Expressions.Source)!);
    const target = translateTarget(node.findDirectExpression(abaplint.Expressions.Target)!);
    return target + ".set(" + source + ");";
  } else if (node.get() instanceof abaplint.Statements.Else) {
    return "} else {";
  }
  return "todo";
}

export function run(code: string): string {
  const file = new abaplint.MemoryFile("zfoobar.prog.abap", code);
  const reg = new abaplint.Registry().addFile(file).parse();
  const abap = reg.getABAPObjects()[0].getABAPFiles()[0];

  let result = "";
  for(const s of abap.getStatements()) {
    result = result + traverseStatement(s);
  }
  return result;
}