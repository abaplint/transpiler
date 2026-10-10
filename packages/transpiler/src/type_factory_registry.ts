import * as abaplint from "@abaplint/core";
import {Chunk} from "./chunk";

/** Collects equivalent constructor expressions and emits one fresh-value factory
 * for each distinct composite constructor in a generated output module. */
export class TypeFactoryRegistry {
  private readonly factories = new Map<string, {name: string, expression: string}>();
  private readonly usedNames = new Set<string>();
  private nextId = 0;
  private readonly modulePrefix: string;
  private readonly inProgress = new WeakSet<abaplint.AbstractType>();
  private readonly resolved = new WeakMap<abaplint.AbstractType, Map<string, string>>();

  public constructor(moduleName: string) {
    // Reversible base64url encoding keeps module names distinct when generated
    // chunks are concatenated, without repeating a long textual prefix per call.
    this.modulePrefix = this.encodeModuleName(moduleName.toLowerCase());
  }

  private encodeModuleName(input: string): string {
    const alphabet = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789_$";
    const bytes = new TextEncoder().encode(input);
    let output = "";
    for (let i = 0; i < bytes.length; i += 3) {
      const a = bytes[i];
      const b = bytes[i + 1];
      const c = bytes[i + 2];
      output += alphabet[Math.floor(a / 4)];
      output += alphabet[(a % 4) * 16 + (b === undefined ? 0 : Math.floor(b / 16))];
      if (b !== undefined) {
        output += alphabet[(b % 16) * 4 + (c === undefined ? 0 : Math.floor(c / 64))];
      }
      if (c !== undefined) {
        output += alphabet[c % 64];
      }
    }
    return output;
  }

  /** Reserve source identifiers before traversal so generated helpers cannot
   * shadow ABAP variables that transpile to JavaScript names. */
  public reserveSource(source: string): void {
    for (const match of source.matchAll(/[A-Za-z_$][A-Za-z0-9_$]*/g)) {
      const identifier = match[0].toLowerCase();
      this.usedNames.add(identifier);
      this.usedNames.add("$" + identifier);
    }
  }

  /** Return a stable factory call for this exact constructor expression. Child
   * composites have already been replaced by their interned factory calls, so
   * the key stays shallow instead of expanding the full nested type. */
  public register(expression: string): string {
    const existing = this.factories.get(expression);
    if (existing !== undefined) {
      return existing.name + "()";
    }

    let name: string;
    do {
      name = "$t_" + this.modulePrefix + "_" + this.nextId++;
    } while (this.usedNames.has(name.toLowerCase()));
    this.usedNames.add(name.toLowerCase());
    this.factories.set(expression, {name, expression});
    return name + "()";
  }

  /** Memoize type-object resolution while still interning equivalent types by
   * their emitted constructor semantics. A recursive type graph is rejected with
   * a bounded diagnostic instead of overflowing the JavaScript stack. */
  public resolveType(type: abaplint.AbstractType, optionsKey: string, render: () => string): string {
    let byOptions = this.resolved.get(type);
    const cached = byOptions?.get(optionsKey);
    if (cached !== undefined) {
      return cached;
    }
    if (this.inProgress.has(type)) {
      throw new Error("Recursive ABAP type construction is not supported: " + type.constructor.name);
    }
    if (byOptions === undefined) {
      byOptions = new Map<string, string>();
      this.resolved.set(type, byOptions);
    }
    this.inProgress.add(type);
    try {
      const result = render();
      byOptions.set(optionsKey, result);
      return result;
    } finally {
      this.inProgress.delete(type);
    }
  }

  /** Inline factories whose declaration and calls cost more than their use-site
   * expressions. This keeps opting in safe for small, mostly unique modules. */
  public finalize(body: Chunk): string {
    let changed = true;
    while (changed) {
      changed = false;
      for (const [key, factory] of this.factories) {
        const call = factory.name + "()";
        let uses = this.count(body.getCode(), call);
        for (const other of this.factories.values()) {
          if (other !== factory) {
            uses += this.count(other.expression, call);
          }
        }

        const inlineBytes = this.utf8Bytes(factory.expression) * uses;
        const helperBytes = this.utf8Bytes("function " + factory.name + "() { return " + factory.expression + "; }\n")
          + this.utf8Bytes(call) * uses;
        if (uses === 0 || helperBytes >= inlineBytes) {
          body.replaceAll(call, factory.expression);
          for (const other of this.factories.values()) {
            if (other !== factory) {
              other.expression = other.expression.split(call).join(factory.expression);
            }
          }
          this.factories.delete(key);
          changed = true;
          break;
        }
      }
    }
    return this.render();
  }

  private count(text: string, search: string): number {
    let count = 0;
    let cursor = 0;
    while (true) {
      const index = text.indexOf(search, cursor);
      if (index < 0) {
        return count;
      }
      count++;
      cursor = index + search.length;
    }
  }

  private utf8Bytes(input: string): number {
    let bytes = 0;
    for (const character of input) {
      const point = character.codePointAt(0)!;
      bytes += point <= 0x7f ? 1 : point <= 0x7ff ? 2 : point <= 0xffff ? 3 : 4;
    }
    return bytes;
  }

  private render(): string {
    let output = "";
    for (const factory of this.factories.values()) {
      output += `function ${factory.name}() { return ${factory.expression}; }\n`;
    }
    return output;
  }

  public get size(): number {
    return this.factories.size;
  }
}

