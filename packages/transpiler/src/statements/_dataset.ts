import * as abaplint from "@abaplint/core";
import {Traversal} from "../traversal";

/** the Sources and Targets of a DATASET statement, each found by the
 *  keywords right in front of it: "MESSAGE" in "FOR INPUT IN BINARY MODE
 *  MESSAGE m", "LENGTH" only when not "MAXIMUM LENGTH" or "ACTUAL LENGTH" */
export class DatasetOperands {
  private readonly operands: {keywords: string[], code: string}[] = [];
  private readonly words: string[] = [];

  public constructor(node: abaplint.Nodes.StatementNode, traversal: Traversal) {
    let keywords: string[] = [];
    for (const c of node.getChildren()) {
      if (c instanceof abaplint.Nodes.TokenNode) {
        keywords.push(c.getFirstToken().getStr().toUpperCase());
        this.words.push(c.getFirstToken().getStr().toUpperCase());
      } else if (c instanceof abaplint.Nodes.ExpressionNode) {
        this.operands.push({keywords, code: traversal.traverse(c).getCode()});
        keywords = [];
      }
    }
  }

  /** whether the statement has these keywords in a row: its own tokens only,
   *  never the text of an operand, so a variable lv_type or a literal
   *  'no end of line' is not an addition */
  public has(words: string): boolean {
    // abaplint splits UTF-8, NON-UNICODE and BYTE-ORDER into three tokens
    const joined = this.words.join(" ").replace(/ - /g, "-");
    return (" " + joined + " ").includes(" " + words + " ");
  }

  /** the dataset name, or the source of TRANSFER: the first operand */
  public first(): string {
    return this.operands[0].code;
  }

  public after(words: string, notAfter: string[] = []): string | undefined {
    const want = words.split(" ");
    for (const o of this.operands.slice(1)) {
      const tail = o.keywords.slice(-want.length);
      const before = o.keywords[o.keywords.length - want.length - 1];
      if (tail.join(" ") === words && (before === undefined || notAfter.includes(before) === false)) {
        return o.code;
      }
    }
    return undefined;
  }
}
