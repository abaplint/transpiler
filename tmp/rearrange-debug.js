const abaplint = require('@abaplint/core');
const abap = process.env.ABAP_SNIPPET ?? `DATA binary_code TYPE x LENGTH 2.
DATA binary_zero TYPE x LENGTH 2.
DATA sdf TYPE x LENGTH 2.
sdf = binary_code BIT-AND CONV xstring( 16 ).`;
const file = new abaplint.MemoryFile('zrearrange.prog.abap', abap);
const reg = new abaplint.Registry().addFiles([file]).parse();
const obj = reg.getFirstObject();
const stru = obj.getMainABAPFile()?.getStructure();
const useRearranger = process.env.USE_REARRANGER === "1";
let workingStructure = stru;
if (useRearranger) {
  const {Rearranger} = require('../packages/transpiler/build/src/rearranger');
  workingStructure = new Rearranger().run(obj.getType(), stru);
}
const source = workingStructure.findFirstStatement(abaplint.Statements.Move).findFirstExpression(abaplint.Expressions.Source);
function print(node, indent = 0) {
  if (!node) return;
  const pad = ' '.repeat(indent);
  const type = node.get() ? node.get().constructor.name : node.constructor.name;
  const token = node.getFirstToken ? node.getFirstToken()?.getStr() : '';
  console.log(`${pad}${node.constructor.name} -> ${type}${token ? ` [${token}]` : ''}`);
  for (const child of node.getChildren()) {
    print(child, indent + 2);
  }
}
print(source);
