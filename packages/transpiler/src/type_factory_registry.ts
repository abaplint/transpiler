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

  /** Matches every call to a factory of this module, the name is the match minus "()" */
  private callPattern(): RegExp {
    const prefix = ("$t_" + this.modulePrefix + "_").replace(/[$]/g, "\\$");
    return new RegExp(prefix + "\\d+\\(\\)", "g");
  }

  private countCalls(text: string, into: Map<string, number>): void {
    for (const match of text.matchAll(this.callPattern())) {
      const name = match[0].slice(0, -2);
      into.set(name, (into.get(name) ?? 0) + 1);
    }
  }

  /** Inline factories whose declaration and calls cost more than their use-site
   * expressions. This keeps opting in safe for small, mostly unique modules.
   *
   * Factories are considered in registration order and the first one worth
   * inlining is inlined, until none is left. Inlining a factory only changes the
   * use counts of the factories it calls and the sizes of the factories calling
   * it, so these are updated in place instead of recounting all text, and the
   * text itself is rewritten once at the end. */
  public finalize(body: Chunk): string {
    type State = {
      factory: {name: string, expression: string},
      order: number,
      uses: number,
      expressionBytes: number,
      callBytes: number,
      declarationBytes: number,
      children: Map<string, number>,
      parents: Set<string>,
      inlined: boolean,
    };

    const states = new Map<string, State>();
    const ordered: State[] = [];
    for (const factory of this.factories.values()) {
      const state: State = {
        factory,
        order: ordered.length,
        uses: 0,
        expressionBytes: this.utf8Bytes(factory.expression),
        callBytes: this.utf8Bytes(factory.name + "()"),
        declarationBytes: this.utf8Bytes("function " + factory.name + "() { return ; }\n"),
        children: new Map<string, number>(),
        parents: new Set<string>(),
        inlined: false,
      };
      states.set(factory.name, state);
      ordered.push(state);
    }

    const bodyCalls = new Map<string, number>();
    this.countCalls(body.getCode(), bodyCalls);
    for (const [name, count] of bodyCalls) {
      const state = states.get(name);
      if (state !== undefined) {
        state.uses += count;
      }
    }
    for (const state of ordered) {
      const calls = new Map<string, number>();
      this.countCalls(state.factory.expression, calls);
      for (const [name, count] of calls) {
        const child = states.get(name);
        if (child !== undefined && child !== state) {
          state.children.set(name, count);
          child.parents.add(state.factory.name);
          child.uses += count;
        }
      }
    }

    const shouldInline = (state: State) => {
      const inlineBytes = state.expressionBytes * state.uses;
      const helperBytes = state.declarationBytes + state.expressionBytes + state.callBytes * state.uses;
      return state.uses === 0 || helperBytes >= inlineBytes;
    };

    // min-heap of registration order, entries are validated when popped
    const heap: number[] = [];
    const push = (value: number) => {
      heap.push(value);
      let index = heap.length - 1;
      while (index > 0) {
        const parent = Math.floor((index - 1) / 2);
        if (heap[parent] <= value) {
          break;
        }
        heap[index] = heap[parent];
        index = parent;
      }
      heap[index] = value;
    };
    const pop = (): number => {
      const top = heap[0];
      const last = heap.pop()!;
      if (heap.length > 0) {
        let index = 0;
        while (true) {
          let child = index * 2 + 1;
          if (child >= heap.length) {
            break;
          }
          if (child + 1 < heap.length && heap[child + 1] < heap[child]) {
            child++;
          }
          if (heap[child] >= last) {
            break;
          }
          heap[index] = heap[child];
          index = child;
        }
        heap[index] = last;
      }
      return top;
    };

    for (const state of ordered) {
      if (shouldInline(state)) {
        push(state.order);
      }
    }

    while (heap.length > 0) {
      const state = ordered[pop()];
      if (state.inlined || !shouldInline(state)) {
        continue;
      }
      state.inlined = true;
      const name = state.factory.name;

      for (const parentName of state.parents) {
        const parent = states.get(parentName)!;
        const occurrences = parent.children.get(name)!;
        parent.children.delete(name);
        parent.expressionBytes += occurrences * (state.expressionBytes - state.callBytes);
        for (const [childName, count] of state.children) {
          parent.children.set(childName, (parent.children.get(childName) ?? 0) + occurrences * count);
          states.get(childName)!.parents.add(parentName);
        }
      }
      for (const [childName, count] of state.children) {
        const child = states.get(childName)!;
        child.uses += count * (state.uses - 1);
        child.parents.delete(name);
      }

      for (const affected of [...state.parents, ...state.children.keys()]) {
        const other = states.get(affected)!;
        if (shouldInline(other)) {
          push(other.order);
        }
      }
    }

    // rewrite all text once, expanding inlined calls recursively
    const expanded = new Map<string, string>();
    const expand = (text: string): string => text.replace(this.callPattern(), call => {
      const state = states.get(call.slice(0, -2));
      if (state === undefined || state.inlined === false) {
        return call;
      }
      let result = expanded.get(state.factory.name);
      if (result === undefined) {
        result = expand(state.factory.expression);
        expanded.set(state.factory.name, result);
      }
      return result;
    });

    body.replaceMatches(this.callPattern(), match => {
      const state = states.get(match[0].slice(0, -2));
      return state?.inlined === true ? expand(match[0]) : undefined;
    });
    for (const [key, factory] of this.factories) {
      if (states.get(factory.name)!.inlined) {
        this.factories.delete(key);
      } else {
        factory.expression = expand(factory.expression);
      }
    }
    return this.render();
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

