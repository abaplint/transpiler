import * as abaplint from "@abaplint/core";
import {Nodes} from "@abaplint/core";
import {IExpressionTranspiler} from "./_expression_transpiler";
import {Traversal} from "../traversal";
import {Chunk} from "../chunk";

export class AttributeNameTranspiler implements IExpressionTranspiler {

  public transpile(node: Nodes.ExpressionNode, traversal: Traversal): Chunk {
    let concat = node.concatTokens().toLowerCase();

    // todo: this needs to also check the class name is correct
    const ref = traversal.findReadOrWriteReference(node.getFirstToken());
    if (ref instanceof abaplint.Types.ClassAttribute
        && ref.getVisibility() === abaplint.Visibility.Private) {
      concat = "#" + concat;
    }

    // an attribute declared in an interface lives on the object as "<intf>$<name>",
    // also when it is reached via an interface reference without the "intf~" alias,
    // eg. "obj->method( )->value" where method returns "REF TO lif_intf"
    const interfaceName = traversal.isInterfaceAttribute(node.getFirstToken());
    if (interfaceName && concat.startsWith(interfaceName) === false) {
      concat = Traversal.escapeNamespace(interfaceName) + "$" + Traversal.escapeNamespace(concat)!.replace("~", "$");
    } else {
      concat = Traversal.escapeNamespace(concat)!.replace("~", "$");
    }

    return new Chunk().append(concat, node, traversal);
  }

}
