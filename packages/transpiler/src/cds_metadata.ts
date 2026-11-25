import * as abaplint from "@abaplint/core";

const cds = abaplint.ExpressionsCDS;

/** An annotation value as JSON: strings, numbers, booleans, null, arrays and records map to their
 * JSON counterparts, an enum value #X is {"#": "X"} and a #( ... ) expression is {"#()": "..."}.
 * "#" is not a CDS name, so neither can be mistaken for a record. */
export type CDSAnnotationValue = string | number | boolean | null | CDSAnnotationValue[] | {[name: string]: CDSAnnotationValue};

export interface ICDSAnnotation {
  /** as written, without the leading @, eg. "EndUserText.label" */
  name: string;
  value: CDSAnnotationValue;
}

export interface ICDSField {
  /** the element name as written, the alias if there is one */
  name: string;
  key: boolean;
  virtual: boolean;
  annotations: ICDSAnnotation[];
}

export interface ICDSCardinality {
  min: number;
  /** "*" for unbounded */
  max: number | "*";
}

export interface ICDSAssociation {
  /** the alias, eg. "_Booking", or the target name if there is no alias */
  name: string;
  /** the target entity, the redirected one in a projection */
  target: string;
  type: "ASSOCIATION" | "COMPOSITION" | "TO_PARENT";
  cardinality?: ICDSCardinality;
  /** the ON condition as written, undefined for compositions and redirections */
  condition?: string;
  defaultFilter?: string;
  /** listed in the element list, ie. part of the entity's signature */
  exposed: boolean;
  /** annotations of the exposing element */
  annotations: ICDSAnnotation[];
}

export interface ICDSMetadata {
  /** VIEW, VIEW_ENTITY, PROJECTION_VIEW, ABSTRACT_ENTITY, CUSTOM_ENTITY, TABLE_FUNCTION, ... */
  definitionType: string;
  root: boolean;
  annotations: ICDSAnnotation[];
  fields: ICDSField[];
  associations: ICDSAssociation[];
}

/** The metadata of a CDS entity, as far as it is written in its own source: its annotations, its
 * elements and its associations. Associations a projection or a view exposes from its source are
 * taken from that source's metadata. Metadata extensions (DDLX) are not merged. */
export class CDSMetadata {
  private readonly cache = new Map<string, ICDSMetadata | undefined>();

  public constructor(private readonly reg: abaplint.IRegistry) {}

  public build(obj: abaplint.Objects.DataDefinition): ICDSMetadata | undefined {
    const key = obj.getName().toUpperCase();
    if (this.cache.has(key)) {
      return this.cache.get(key);
    }
    this.cache.set(key, undefined); // cycle guard, associations can point back
    const tree = obj.getTree() ?? (obj.parse(), obj.getTree());
    const result = tree === undefined ? undefined : this.definition(tree);
    this.cache.set(key, result);
    return result;
  }

  private definition(root: abaplint.Nodes.ExpressionNode): ICDSMetadata {
    const result: ICDSMetadata = {
      definitionType: this.definitionType(root),
      root: root.findDirectTokenByText("ROOT") !== undefined,
      annotations: [],
      fields: [],
      associations: [],
    };

    for (const child of root.getChildren()) {
      if (child.get() instanceof cds.CDSAnnotation) {
        result.annotations.push(CDSMetadata.annotation(child as abaplint.Nodes.ExpressionNode));
      } else {
        break;
      }
    }

    const def = root.get();
    if (def instanceof cds.CDSDefineView || def instanceof cds.CDSDefineProjection) {
      const select = def instanceof cds.CDSDefineView ? root.findFirstExpression(cds.CDSSelect) : root;
      if (select !== undefined) {
        for (const a of select.findDirectExpressions(cds.CDSAssociation)) {
          result.associations.push(this.association(a, "ASSOCIATION"));
        }
        for (const c of select.findDirectExpressions(cds.CDSComposition)) {
          result.associations.push(this.association(c, "COMPOSITION"));
        }
        for (const e of select.findDirectExpressions(cds.CDSElement)) {
          this.element(e, result, this.sources(root));
        }
      }
    } else if (def instanceof cds.CDSDefineHierarchy) {
      this.hierarchy(root, result);
    } else {
      this.signature(root, result);
    }
    return result;
  }

  private definitionType(root: abaplint.Nodes.ExpressionNode): string {
    const def = root.get();
    if (def instanceof cds.CDSDefineView) {
      return root.findDirectTokenByText("ENTITY") ? "VIEW_ENTITY" : "VIEW";
    } else if (def instanceof cds.CDSDefineProjection) {
      return "PROJECTION_VIEW";
    } else if (def instanceof cds.CDSDefineAbstract) {
      return "ABSTRACT_ENTITY";
    } else if (def instanceof cds.CDSDefineCustom) {
      return "CUSTOM_ENTITY";
    } else if (def instanceof cds.CDSDefineTableFunction) {
      return "TABLE_FUNCTION";
    } else if (def instanceof cds.CDSDefineHierarchy) {
      return "HIERARCHY";
    } else if (def instanceof cds.CDSDefineTableEntity) {
      return "TABLE_ENTITY";
    } else if (def instanceof cds.CDSDefineExternalEntity) {
      return "EXTERNAL_ENTITY";
    }
    return def.constructor.name;
  }

  /** The entities selected or projected from, for the associations they expose */
  private sources(root: abaplint.Nodes.ExpressionNode): string[] {
    const ret: string[] = [];
    if (root.get() instanceof cds.CDSDefineProjection) {
      // the first name is the projection's own, the second the projected entity
      const name = root.findDirectExpressions(cds.CDSName)[1];
      if (name) {
        ret.push(name.concatTokens().toUpperCase());
      }
    }
    for (const s of root.findAllExpressions(cds.CDSSource)) {
      const name = s.findDirectExpression(cds.CDSPrefixedName) ?? s.getFirstChild();
      if (name) {
        ret.push(name.concatTokens().replace(/ /g, "").toUpperCase());
      }
    }
    return ret;
  }

  private sourceAssociation(sources: string[], name: string): ICDSAssociation | undefined {
    for (const source of sources) {
      const obj = this.reg.getObject("DDLS", source) as abaplint.Objects.DataDefinition | undefined;
      const found = obj ? this.build(obj)?.associations.find(a => a.name.toUpperCase() === name.toUpperCase()) : undefined;
      if (found) {
        return found;
      }
    }
    return undefined;
  }

  private element(e: abaplint.Nodes.ExpressionNode, result: ICDSMetadata, sources: string[]): void {
    const annotations = e.findDirectExpressions(cds.CDSAnnotation).map(a => CDSMetadata.annotation(a));
    const prefixed = e.findDirectExpression(cds.CDSPrefixedName);
    const path = prefixed?.findAllExpressions(cds.CDSName).map(n => n.concatTokens()) ?? [];
    const as = e.findDirectExpressions(cds.CDSAs).map(a => this.alias(a)).find(a => a !== undefined);
    const redirected = this.redirection(e) ?? e.findDirectExpressions(cds.CDSAs).map(a => this.redirection(a)).find(r => r);

    let name = as;
    if (name === undefined && path.length > 0) {
      name = redirected ? path[0] : path[path.length - 1];
    }
    if (name === undefined && e.findDirectTokenByText("VIRTUAL")) {
      name = e.findFirstExpression(cds.CDSName)?.concatTokens();
    }
    if (name === undefined) {
      return;
    }

    // an exposed association, declared here or in a source
    const exposes = path.length > 0 ? path[path.length - 1] : name;
    const declared = path.length === 1
      ? result.associations.find(a => a.name.toUpperCase() === exposes.toUpperCase()) : undefined;
    if (declared) {
      declared.exposed = true;
      declared.annotations.push(...annotations);
      if (redirected) {
        declared.target = redirected.target;
      }
      return;
    }
    const inherited = redirected !== undefined || path.length > 0 ? this.sourceAssociation(sources, exposes) : undefined;
    if (inherited || redirected) {
      result.associations.push({
        name: name,
        target: redirected?.target ?? inherited!.target,
        type: redirected?.type ?? inherited!.type,
        cardinality: inherited?.cardinality,
        condition: inherited?.condition,
        defaultFilter: inherited?.defaultFilter,
        exposed: true,
        annotations: annotations,
      });
      return;
    }

    result.fields.push({
      name: name,
      key: e.findDirectTokenByText("KEY") !== undefined,
      virtual: e.findDirectTokenByText("VIRTUAL") !== undefined,
      annotations: annotations,
    });
  }

  private alias(as: abaplint.Nodes.ExpressionNode): string | undefined {
    const ret: string[] = [];
    for (const c of as.getChildren().slice(1)) {
      if (c.concatTokens().startsWith(":")) {
        break;
      }
      ret.push(c.concatTokens());
    }
    return ret.length > 0 ? ret.join("") : undefined;
  }

  /** ": REDIRECTED TO [PARENT | COMPOSITION CHILD] target", in an element or its alias */
  private redirection(node: abaplint.Nodes.ExpressionNode): {target: string, type: ICDSAssociation["type"]} | undefined {
    const children = node.getChildren();
    const to = children.findIndex(c => c instanceof abaplint.Nodes.TokenNode && c.getFirstToken().getStr().toUpperCase() === "TO"
      && children.some(r => r instanceof abaplint.Nodes.TokenNode && r.getFirstToken().getStr().toUpperCase() === "REDIRECTED"));
    if (to < 0) {
      return undefined;
    }
    const rest = children.slice(to + 1).map(c => c.concatTokens().toUpperCase());
    const target = children.slice(to + 1).find(c => c.get() instanceof cds.CDSName)?.concatTokens();
    if (target === undefined) {
      return undefined;
    }
    let type: ICDSAssociation["type"] = "ASSOCIATION";
    if (rest.includes("PARENT")) {
      type = "TO_PARENT";
    } else if (rest.includes("CHILD")) {
      type = "COMPOSITION";
    }
    return {target, type};
  }

  /** hierarchies list their elements directly under the root, after the "{" and separated by ",":
   * [annotations] [KEY] path [AS alias]. Associations of the SOURCE entity can be exposed. */
  private hierarchy(root: abaplint.Nodes.ExpressionNode, result: ICDSMetadata): void {
    const children = root.getChildren();
    const token = (c: abaplint.Nodes.ExpressionNode | abaplint.Nodes.TokenNode) =>
      c instanceof abaplint.Nodes.TokenNode ? c.getFirstToken().getStr().toUpperCase() : "";
    const source = children[children.findIndex(c => token(c) === "SOURCE") + 1];
    const sources = source?.get() instanceof cds.CDSName ? [source.concatTokens().toUpperCase()] : [];

    let annotations: ICDSAnnotation[] = [];
    let key = false;
    let path: string[] = [];
    let as: string | undefined;
    for (const child of children.slice(children.findIndex(c => token(c) === "{") + 1)) {
      const text = token(child);
      if (child.get() instanceof cds.CDSAnnotation) {
        annotations.push(CDSMetadata.annotation(child as abaplint.Nodes.ExpressionNode));
      } else if (text === "KEY") {
        key = true;
      } else if (child.get() instanceof cds.CDSPrefixedName) {
        path = (child as abaplint.Nodes.ExpressionNode).findAllExpressions(cds.CDSName).map(n => n.concatTokens());
      } else if (child.get() instanceof cds.CDSAs) {
        as = this.alias(child as abaplint.Nodes.ExpressionNode);
      } else if (text === "," || text === "}") {
        if (path.length > 0) {
          const name = as ?? path[path.length - 1];
          const inherited = this.sourceAssociation(sources, path[path.length - 1]);
          if (inherited) {
            result.associations.push({...inherited, name, exposed: true, annotations});
          } else {
            result.fields.push({name, key, virtual: false, annotations});
          }
        }
        annotations = [];
        key = false;
        path = [];
        as = undefined;
        if (text === "}") {
          return;
        }
      }
    }
  }

  /** abstract and custom entities and table functions list their elements inline: name : type ; */
  private signature(root: abaplint.Nodes.ExpressionNode, result: ICDSMetadata): void {
    let inBody = false;
    let annotations: ICDSAnnotation[] = [];
    let key = false;
    let name: string | undefined;
    let assoc: abaplint.Nodes.ExpressionNode | undefined;

    for (const child of root.getChildren()) {
      const text = child instanceof abaplint.Nodes.TokenNode ? child.getFirstToken().getStr().toUpperCase() : "";
      if (inBody === false) {
        inBody = text === "{";
        continue;
      }
      if (child.get() instanceof cds.CDSAnnotation) {
        annotations.push(CDSMetadata.annotation(child as abaplint.Nodes.ExpressionNode));
      } else if (text === "KEY") {
        key = true;
      } else if (child.get() instanceof cds.CDSName && name === undefined) {
        name = child.concatTokens();
      } else if (child.get() instanceof cds.CDSAssociation || child.get() instanceof cds.CDSComposition) {
        assoc = child as abaplint.Nodes.ExpressionNode;
      } else if (text === ";" || text === "}") {
        if (name !== undefined && assoc !== undefined) {
          const a = this.association(assoc, assoc.get() instanceof cds.CDSComposition ? "COMPOSITION" : "ASSOCIATION");
          a.name = name;
          a.exposed = true;
          a.annotations = annotations;
          result.associations.push(a);
        } else if (name !== undefined) {
          result.fields.push({name, key, virtual: false, annotations});
        }
        annotations = [];
        key = false;
        name = undefined;
        assoc = undefined;
        if (text === "}") {
          return;
        }
      }
    }
  }

  private association(node: abaplint.Nodes.ExpressionNode, type: ICDSAssociation["type"]): ICDSAssociation {
    const relation = node.findDirectExpression(cds.CDSRelation);
    const skip = new Set<abaplint.Nodes.ExpressionNode | abaplint.Nodes.TokenNode>();
    relation?.findDirectExpressions(cds.CDSAs).forEach(a => skip.add(a));
    relation?.findDirectExpressions(cds.CDSParametersSelect).forEach(a => skip.add(a));
    const target = relation?.getChildren().filter(c => !skip.has(c)).map(c => c.concatTokens().replace(/ /g, "")).join("") || "";
    const as = relation?.findDirectExpression(cds.CDSAs);
    const conditions = node.findDirectExpressions(cds.CDSCondition);

    // the words before the target: ASSOCIATION [0..1] TO, ASSOCIATION OF MANY TO ONE, COMPOSITION OF ONE TO MANY, ...
    const words: string[] = [];
    for (const c of node.getChildren()) {
      if (c === relation) {
        break;
      }
      words.push(...c.concatTokens().toUpperCase().split(/\s+|(?=\[|\]|\.\.)|(?<=\[|\]|\.\.)/).filter(w => w !== ""));
    }
    if (words.includes("PARENT")) {
      type = "TO_PARENT";
    }

    return {
      name: (as ? this.alias(as) : undefined) ?? target,
      target: target,
      type: type,
      cardinality: this.cardinality(words, type),
      condition: conditions[0] ? CDSMetadata.text(conditions[0]) : undefined,
      defaultFilter: conditions[1] ? CDSMetadata.text(conditions[1]) : undefined,
      exposed: false,
      annotations: [],
    };
  }

  private cardinality(words: string[], type: ICDSAssociation["type"]): ICDSCardinality {
    if (type === "TO_PARENT") {
      return {min: 1, max: 1};
    }
    const open = words.indexOf("[");
    if (open >= 0) {
      const inside = words.slice(open + 1, words.indexOf("]")).join("").split("..");
      const num = (s: string) => s === "*" ? "*" as const : parseInt(s, 10);
      // [n] is [0..n]
      return inside.length === 1 ? {min: 0, max: num(inside[0])} : {min: num(inside[0]) as number, max: num(inside[1])};
    }
    // the target side is the last one: TO ONE, OF MANY TO ONE, OF EXACT ONE, ONE TO MANY TO
    const sides = words.filter(w => w !== "ASSOCIATION" && w !== "COMPOSITION" && w !== "OF")
      .join(" ").split("TO").map(s => s.trim()).filter(s => s !== "");
    const side = sides[sides.length - 1];
    if (side === "EXACT ONE") {
      return {min: 1, max: 1};
    } else if (side === "ONE") {
      return {min: 0, max: 1};
    } else if (side === "MANY") {
      return {min: 0, max: "*"};
    }
    return type === "COMPOSITION" ? {min: 0, max: "*"} : {min: 0, max: 1};
  }

  ///////////////////////////////

  /** the source text, tokens separated by single spaces except around path dots: $projection.id = _Item.id */
  private static text(node: abaplint.Nodes.ExpressionNode): string {
    let ret = "";
    let previous = "";
    for (const token of node.getTokens().map(t => t.getStr())) {
      ret += (ret === "" || token === "." || previous === "." ? "" : " ") + token;
      previous = token;
    }
    return ret;
  }

  public static annotation(node: abaplint.Nodes.ExpressionNode): ICDSAnnotation {
    const children = node.getChildren();
    const colon = children.findIndex(c => c instanceof abaplint.Nodes.TokenNode && c.getFirstToken().getStr() === ":");
    const nameParts = (colon < 0 ? children : children.slice(0, colon)).map(c => c.concatTokens());
    const name = nameParts.join("").replace(/\s/g, "").replace(/^@<?/, "");
    const valueNode = colon < 0 ? undefined : children[colon + 1];
    // an annotation without a value is true
    const value = valueNode instanceof abaplint.Nodes.ExpressionNode ? CDSMetadata.value(valueNode) : true;
    return {name, value};
  }

  private static value(node: abaplint.Nodes.ExpressionNode): CDSAnnotationValue {
    const type = node.get();
    if (type instanceof cds.CDSAnnotationArray) {
      return node.getChildren()
        .filter((c): c is abaplint.Nodes.ExpressionNode => c instanceof abaplint.Nodes.ExpressionNode)
        .map(c => CDSMetadata.value(c));
    } else if (type instanceof cds.CDSAnnotationObject) {
      const record: {[name: string]: CDSAnnotationValue} = {};
      let path: string[] = [];
      let assigned = false;
      for (const c of node.getChildren()) {
        if (c.get() instanceof cds.CDSName) {
          path.push(c.concatTokens());
        } else if (c instanceof abaplint.Nodes.ExpressionNode) {
          CDSMetadata.set(record, path, CDSMetadata.value(c));
          assigned = true;
        } else {
          const text = c.getFirstToken().getStr();
          if ((text === "," || text === "}") && path.length > 0) {
            if (assigned === false) {
              CDSMetadata.set(record, path, true);
            }
            path = [];
            assigned = false;
          }
        }
      }
      return record;
    }
    return CDSMetadata.simple(node);
  }

  private static simple(node: abaplint.Nodes.ExpressionNode): CDSAnnotationValue {
    const tokens = node.getTokens().map(t => t.getStr());
    const first = tokens[0] ?? "";
    if (first.startsWith("'")) {
      return first.substring(1, first.length - 1).replace(/''/g, "'");
    } else if (first === "#" && tokens[1] === "(") {
      return {"#()": tokens.slice(2, tokens.length - 1).join(" ")};
    } else if (first.startsWith("#")) {
      return {"#": first.substring(1)};
    }
    const lower = tokens.join("").toLowerCase();
    if (lower === "true" || lower === "false") {
      return lower === "true";
    } else if (lower === "null") {
      return null;
    }
    const num = Number(tokens.join(""));
    return isNaN(num) ? tokens.join("") : num;
  }

  private static set(record: {[name: string]: CDSAnnotationValue}, path: string[], value: CDSAnnotationValue): void {
    let current = record;
    for (const part of path.slice(0, path.length - 1)) {
      const next = current[part];
      if (typeof next !== "object" || next === null || Array.isArray(next)) {
        current[part] = {};
      }
      current = current[part] as {[name: string]: CDSAnnotationValue};
    }
    current[path[path.length - 1]] = value;
  }
}
