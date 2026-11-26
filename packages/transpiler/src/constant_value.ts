import * as abaplint from "@abaplint/core";

/** Where the value of `VALUE other=>constant` comes from, for a constant of an interface.
 * The chain is followed to the constant that declares a value:
 * - literal: it ends in an interface constant with a value. Interfaces run no code, so the
 *   compile-time value is the value, and there is no load-time edge, also when two interfaces
 *   name each other's constants;
 * - member: it ends in a class constant, which the class may set at run time (a kernel class
 *   sets cl_abap_char_utilities=>newline in its class constructor), or runs into an object no
 *   analysis knows. The value is read at load from that member, and the module requires its
 *   object first. A class module never imports an interface, so an interface's edge to a class
 *   cannot close a cycle;
 * - void: the referenced object is not in the registry; the caller keeps its field chain.
 * A component chain (`zif_a=>gs-comp`) folds to, or reads, the component, never the structure.
 * A chain that comes back to a constant it already passed never reaches a value (illegal ABAP,
 * which abaplint accepts): it is refused with the chain named, since reading it at load would
 * make the interfaces await each other and the modules would never finish loading.
 * `path` holds the constants followed so far. */
export type ConstantSource =
  {kind: "literal", value: string} |
  {kind: "member", object: abaplint.ABAPObject, attribute: string, components: string[]} |
  {kind: "void"};

export function constantSource(reg: abaplint.IRegistry, chain: abaplint.Nodes.ExpressionNode,
                               path: string[] = []): ConstantSource {
  const match = /^([^\s=]+)\s*=>\s*([^\s-]+)(?:\s*-\s*(.+))?$/.exec(chain.concatTokens());
  if (match === null) {
    return {kind: "void"};
  }
  let objectName = match[1];
  let attribute = match[2];
  if (attribute.includes("~")) {
    [objectName, attribute] = attribute.split("~");
  }
  const components = match[3]?.split(/\s*-\s*/).map(c => c.toLowerCase()) ?? [];
  const obj = reg.getObject("INTF", objectName) || reg.getObject("CLAS", objectName);
  if (!(obj instanceof abaplint.ABAPObject)) {
    return {kind: "void"};
  }
  const key = (objectName + "=>" + attribute).toUpperCase();
  if (path.includes(key)) {
    throw new Error((obj.getMainABAPFile()?.getFilename() ?? obj.getName()) + ": constant chain "
      + [...path, key].join(" -> ") + " never reaches a value");
  }
  const member = {kind: "member" as const, object: obj, attribute: attribute, components: components};
  if (obj.getType() === "CLAS") {
    // a class's constant may change at run time
    return member;
  }

  const identifier = findConstant(reg, obj, attribute);
  if (identifier === undefined) {
    return member;
  }
  if (components.length > 0) {
    let value: any = identifier.getValue();
    for (const component of components) {
      const name = typeof value === "object" && value !== null
        ? Object.keys(value).find(k => k.toLowerCase() === component) : undefined;
      value = name === undefined ? undefined : value[name];
    }
    return typeof value === "string" ? {kind: "literal", value: value} : member;
  }
  const next = findStatement(obj, identifier)?.findDirectExpression(abaplint.Expressions.Value)?.getChildren()[1];
  if (next instanceof abaplint.Nodes.ExpressionNode && next.get() instanceof abaplint.Expressions.SimpleFieldChain) {
    const further = constantSource(reg, next, [...path, key]);
    return further.kind === "void" ? member : further;
  }
  const value = identifier.getValue();
  return typeof value === "string" ? {kind: "literal", value: value} : member;
}

function findConstant(reg: abaplint.IRegistry, obj: abaplint.ABAPObject, attribute: string): abaplint.TypedIdentifier | undefined {
  const found: abaplint.TypedIdentifier[] = [];
  const walk = (n: abaplint.ISpaghettiScopeNode) => {
    const id = n.getIdentifier();
    if (id.stype === abaplint.ScopeType.Interface && id.sname.toUpperCase() === obj.getName().toUpperCase()) {
      const v = n.findVariable(attribute);
      if (v !== undefined) {
        found.push(v);
      }
    }
    n.getChildren().forEach(walk);
  };
  walk(new abaplint.SyntaxLogic(reg, obj).run().spaghetti.getTop());
  return found[0];
}

function findStatement(obj: abaplint.ABAPObject, identifier: abaplint.TypedIdentifier): abaplint.Nodes.StatementNode | undefined {
  const file = obj.getABAPFiles().find(f => f.getFilename() === identifier.getFilename());
  const start = identifier.getStart();
  return file?.getStatements().find(s => s.get() instanceof abaplint.Statements.Constant
    && s.getStart().getRow() <= start.getRow() && s.getEnd().isAfter(start));
}

/** the JS expression reading a member source at load */
export function memberRead(source: {object: abaplint.ABAPObject, attribute: string, components: string[]}): string {
  const name = source.object.getName();
  const attribute = source.attribute.toLowerCase();
  const property = source.object.getType() === "INTF"
    ? name.toLowerCase().replace(/\//g, "$") + "$" + attribute
    : attribute;
  return "abap.Classes['" + name.toUpperCase() + "']." + property.replace(/\//g, "$")
    + source.components.map(c => c.includes("/") ? ".get()[\"" + c + "\"]" : ".get()." + c).join("");
}
