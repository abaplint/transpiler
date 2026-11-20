import {FieldSymbol} from "./field_symbol";
import {HashedTable, Table} from "./table";
import {ICharacter} from "./_character";
import {INumeric} from "./_numeric";
import {parse} from "../operators/_parse";
import {Hex} from "./hex";
import {Character} from "./character";
import {throwError} from "../throw_error";
import {ABAPObject} from "./abap_object";
import {Numc} from "./numc";
import {Date} from "./date";
import {Time} from "./time";
import {String} from "./string";
import {XString} from "./xstring";
import {DataReference} from "./data_reference";

type CharLike = Character | Numc | Date | Time;
type Fragment = {chars: CharLike[], length: number} | {other: any};

/** length of a flat character-like field (c, n, d, t), undefined for every other type */
function charLength(v: any): number | undefined {
  if (v instanceof Character || v instanceof Numc) {
    return v.getLength();
  } else if (v instanceof Date) {
    return 8;
  } else if (v instanceof Time) {
    return 6;
  }
  return undefined;
}

/** true when two elementary components at the same position may not move one to one: a
 *  character-like one facing another type, or c / n of another length */
function differentCharLayout(source: any, target: any): boolean {
  if (source === undefined || source === null) {
    return false;
  } else if (source.constructor !== target.constructor) {
    return charLength(source) !== undefined || charLength(target) !== undefined;
  }
  return (target instanceof Character || target instanceof Numc)
    && target.getLength() !== source.getLength();
}

/** fragment view of a flat structure: consecutive character-like components, also across
 *  nested structures, form one character fragment; returns false for a deep structure */
function fragments(s: Structure, out: Fragment[]): boolean {
  const v = s.get();
  for (const key of Object.keys(v)) {
    const val = v[key];
    if (val instanceof Structure) {
      if (fragments(val, out) === false) {
        return false;
      }
      continue;
    }
    const length = charLength(val);
    if (length !== undefined) {
      const last = out[out.length - 1];
      if (last !== undefined && "chars" in last) {
        last.chars.push(val);
        last.length += length;
      } else {
        out.push({chars: [val], length});
      }
    } else if (val instanceof Table || val instanceof HashedTable || val instanceof String
        || val instanceof XString || val instanceof ABAPObject || val instanceof DataReference) {
      return false;
    } else {
      out.push({other: val});
    }
  }
  return true;
}

/** a move between flat structures that are not compatible goes fragment by fragment, a
 *  character fragment is moved like one c field, filled with blanks; returns false, and
 *  changes nothing, when the fragment views do not match */
function moveByFragments(source: Structure, target: Structure): boolean {
  const sf: Fragment[] = [];
  const tf: Fragment[] = [];
  if (fragments(source, sf) === false || fragments(target, tf) === false) {
    return false;
  }
  const count = Math.min(sf.length, tf.length);
  for (let i = 0; i < count; i++) {
    const s = sf[i];
    const t = tf[i];
    if ("chars" in s !== "chars" in t) {
      return false;
    } else if ("chars" in s && "chars" in t && s.length !== t.length && i !== count - 1) {
      // only the last fragment may differ in length
      return false;
    }
  }

  for (let i = 0; i < count; i++) {
    const s = sf[i];
    const t = tf[i];
    if ("chars" in s && "chars" in t) {
      let text = s.chars.map(c => c.get()).join("");
      text = text.length > t.length ? text.substring(0, t.length) : text.padEnd(t.length, " ");
      let pos = 0;
      for (const c of t.chars) {
        const length = charLength(c)!;
        c.set(text.substring(pos, pos + length));
        pos += length;
      }
    } else if ("other" in s && "other" in t) {
      t.other.set(s.other);
    }
  }
  return true;
}

// structures are typically instantiated repeatedly with the same literal names,
// cache the uppercasing
const upperCaseCache = new Map<string, string>();
function cachedUpperCase(name: string): string {
  let upper = upperCaseCache.get(name);
  if (upper === undefined) {
    upper = name.toUpperCase();
    upperCaseCache.set(name, upper);
  }
  return upper;
}

export class Structure {
  private readonly value: {[key: string]: any};
  private readonly qualifiedName: string | undefined;
  private readonly ddicName: string | undefined;
  private readonly suffix: { [key: string]: string };
  private readonly asInclude: { [key: string]: boolean };

  // "cloning = true" is only used from clone(), the names are then already uppercased
  public constructor(fields: {[key: string]: any}, qualifiedName?: string, ddicName?: string,
    suffix?: any, asInclude?: any, cloning = false) {

    this.value = fields;
    if (cloning === true) {
      this.qualifiedName = qualifiedName;
      this.ddicName = ddicName;
    } else {
      this.qualifiedName = qualifiedName === undefined ? undefined : cachedUpperCase(qualifiedName);
      this.ddicName = ddicName === undefined ? undefined : cachedUpperCase(ddicName);
    }
    this.suffix = suffix;
    this.asInclude = asInclude;

    if (asInclude !== undefined) {
      this.linkGroupFields();
    }
  }

  private linkGroupFields() {
    for (const as of Object.keys(this.asInclude || {})) {
      const suffix = this.suffix?.[as] || "";
      for (const fieldName of Object.keys(this.value[as].get())) {
        this.value[fieldName + suffix] = this.value[as].get()[fieldName];
      }
    }
  }

  public clone(): Structure {
    const newValues: {[key: string]: any} = {};
    for (const key in this.value) {
      newValues[key] = this.value[key].clone();
    }

    // note: when includes are present, the constructor re-links the group
    // fields so they alias the cloned include's fields
    const n = new Structure(newValues, this.qualifiedName, this.ddicName, this.suffix, this.asInclude, true);
    return n;
  }

  public clear() {
    for (const f in this.value) {
      this.value[f].clear();
    }
    return this;
  }

  public getDDICName() {
    return this.ddicName;
  }

  public getRenamingSuffix() {
    return this.suffix;
  }

  public getAsInclude() {
    return this.asInclude;
  }

  public getQualifiedName() {
    return this.qualifiedName;
  }

  public setField(name: string, value: any) {
    if (name.includes("->") || name.includes("=>")) {
      throw new Error("transpiler, structure setField todo, " + name);
    }

    if (name.includes("-")) {
      // hmm, todo
      const [base, field] = name.split("-");
      if (field.includes("-")) {
        throw new Error("transpiler, structure setField todo");
      }
      this.value[base].setField(field, value);
    } else {
      if (this.value[name] === undefined) {
        throw new Error("Structure, setField, field not found: " + name);
      }
      this.value[name].set(value);
    }
    return this;
  }

  public set(input: Structure | string | INumeric | Table | ICharacter | FieldSymbol | undefined) {
    if (input === undefined) {
      return;
    }

    if (input instanceof FieldSymbol) {
      this.set(input.getPointer());
    } else if (input instanceof Table) {
      throw new Error("Structure, input is a table");
    } else if (input instanceof Structure) {
      if (input === this) {
        return this;
      }
      const obj = input.get();
      const keys1 = Object.keys(obj);
      const keys2 = Object.keys(this.value);
      // the components do not line up one to one, the character fragments may not either
      let fragmentsTried = keys1.length !== keys2.length && moveByFragments(input, this);
      if (fragmentsTried === true) {
        return this;
      }
      for (let i = 0; i < keys1.length; i++) {
        const key1 = keys1[i];
        const key2 = keys2[i];
        const target = this.value[key2];
        if (target === undefined) {
          break;
        }
        const val = obj[key1];
        if (val instanceof Structure || val instanceof Table || val instanceof HashedTable) {
          // deep types are cloned so the target never shares references with the source
          target.set(val.clone());
          continue;
        } else if (fragmentsTried === false && differentCharLayout(val, target)) {
          // a character-like component of another length or type, the move goes by fragments,
          // which also redoes the components already moved, they line up
          if (moveByFragments(input, this) === true) {
            return this;
          }
          fragmentsTried = true;
        }
        // elementary types copy by value in set(), no clone needed
        target.set(val);
      }
    } else {
      this.setCharacter(input);
    }

    return this;
  }

  private setCharacter(input: string | ICharacter | INumeric) {
    this.clear();

    let val = input;
    if (typeof val !== "string") {
      val = val.get() + "";
    }

    for (const key of Object.keys(this.value)) {
      const targetLength = this.value[key].getLength();
      this.value[key].set(val.substr(0, targetLength));
      val = val.substr(targetLength);
    }
  }

  public get() {
    return this.value;
  }

  public getCharacter(allowObject = false): string {
    let val = "";
    for (const v in this.value) {
      const foo = this.value[v];
      if (foo instanceof Structure) {
        val += foo.getCharacter(allowObject);
      } else if (foo instanceof ABAPObject) {
        if (allowObject === false) {
          throw new Error("Structure getCharacter: unexpected ABAPObject");
        }
        val += foo.getInternalID();
      } else {
        val += foo.get();
      }
    }
    return val;
  }

  public getOffset(input: {offset?: number | INumeric | Hex, length?: number | INumeric | Hex}) {
    let offset = input?.offset;
    if (offset) {
      offset = parse(offset);
    }

    let length = input?.length;
    if (length) {
      length = parse(length);
    }

    const val = this.getCharacter();

    if ((offset && offset >= val.length)
         || (offset && offset < 0)
         || (length && length < 0)) {
      throwError("CX_SY_RANGE_OUT_OF_BOUNDS");
    }

    let ret = val;
    if (offset) {
      ret = ret.substr(offset);
    }
    if (length !== undefined) {
      ret = ret.substr(0, length);
    }
    const r = new Character(ret.length);
    r.set(ret);
    return r;
  }
}
