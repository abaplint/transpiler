import {Context} from "../context";
import {throwError} from "../throw_error";
import {Character, FieldSymbol, Hex, HexUInt8, String, XString} from "../types";
import {ICharacter} from "../types/_character";
import {INumeric} from "../types/_numeric";
import {ABAP} from "..";
import {DatasetHost, IGetDatasetOptions, IOpenDatasetOptions, IReadDatasetOptions, ITransferOptions, OpenDataset} from "../dataset/dataset";

declare const abap: ABAP;

// OPEN / READ / TRANSFER / CLOSE / DELETE / GET / SET DATASET.
//
// The ABAP semantics live here, once: text lines, the byte layout of each
// mode, ACTUAL LENGTH, sy-subrc and the exceptions. A host supplies bytes
// only (DatasetHost), the way a DatabaseClient supplies rows, so that
// a sandboxed file system, an in-memory one in a browser and a test double
// all behave the same. Without a host every statement throws, as before.
// The host and the options are defined in ../dataset/dataset.ts, and an
// in-memory host is ../dataset/memory_dataset.ts.

// what the text mode reads per host call while it looks for the end of a line
const CHUNK = 64 * 1024;

function deref<T>(value: T | FieldSymbol): T {
  if (value instanceof FieldSymbol) {
    const pointer = value.getPointer();
    if (pointer === undefined) {
      throw new Error("GETWA_NOT_ASSIGNED");
    }
    return pointer as T;
  }
  return value;
}

function nameOf(name: ICharacter | FieldSymbol | string): string {
  const value = typeof name === "string" ? name : deref(name).get();
  // a C field holding the name is padded; the name is not
  return (value + "").replace(/ +$/, "");
}

function numberOf(value: INumeric | FieldSymbol | undefined): number | undefined {
  return value === undefined ? undefined : Number(deref(value).get());
}

function setSubrc(value: number) {
  abap.builtin.sy.get().subrc.set(value);
}

function isHex(target: any): boolean {
  return target instanceof Hex || target instanceof HexUInt8;
}

function isCharLike(target: any): boolean {
  return typeof target?.get === "function" && typeof target.get() === "string" && !isHex(target) && !(target instanceof XString);
}

function hexToBytes(hex: string): Uint8Array {
  const out = new Uint8Array(Math.floor(hex.length / 2));
  for (let i = 0; i < out.length; i++) {
    out[i] = parseInt(hex.substr(i * 2, 2), 16);
  }
  return out;
}

function bytesToHex(bytes: Uint8Array): string {
  let out = "";
  for (const b of bytes) {
    out += b.toString(16).toUpperCase().padStart(2, "0");
  }
  return out;
}

// a character-like field in BINARY MODE is its code units in the system code
// page, UTF-16LE on a Unicode system (measured: c(4) 'ab' is 6100 6200
// 2000 2000)
function utf16le(text: string): Uint8Array {
  const out = new Uint8Array(text.length * 2);
  for (let i = 0; i < text.length; i++) {
    const code = text.charCodeAt(i);
    out[i * 2] = code % 256;
    out[i * 2 + 1] = Math.floor(code / 256);
  }
  return out;
}

function fromUtf16le(bytes: Uint8Array): string {
  let out = "";
  for (let i = 0; i + 1 < bytes.length; i += 2) {
    out += globalThis.String.fromCharCode(bytes[i] + bytes[i + 1] * 256);
  }
  return out;
}

function concat(a: Uint8Array, b: Uint8Array): Uint8Array<ArrayBufferLike> {
  const out = new Uint8Array(a.length + b.length);
  out.set(a, 0);
  out.set(b, a.length);
  return out;
}

export class DatasetStatements {
  private readonly context: Context;

  public constructor(context: Context) {
    this.context = context;
  }

  private host(statement: string): DatasetHost {
    if (this.context.dataset === undefined) {
      throw new Error(`${statement}, not supported: no dataset host installed (abap.context.dataset)`);
    }
    return this.context.dataset;
  }

  private opened(name: string): OpenDataset {
    const file = this.context.datasets[name];
    if (file === undefined) {
      throwError("CX_SY_FILE_OPEN_MODE");
    }
    return file;
  }

  public async openDataset(nameIn: ICharacter | FieldSymbol | string, options: IOpenDatasetOptions): Promise<void> {
    const host = this.host("OPEN DATASET");
    if (options.legacy === true || options.encoding === "NON-UNICODE") {
      throw new Error("OPEN DATASET, LEGACY and NON-UNICODE modes not supported");
    }
    if (options.unsupported !== undefined && options.unsupported.length > 0) {
      throw new Error(`OPEN DATASET, not supported: ${options.unsupported.join(", ")}`);
    }
    const name = nameOf(nameIn);
    if (this.context.datasets[name] !== undefined) {
      throwError("CX_SY_FILE_OPEN");
    }
    const opened = await host.open(name, options.mode);
    if (!("read" in opened)) {
      if (options.message !== undefined) {
        deref(options.message).set(opened.message);
      }
      setSubrc(8);
      return;
    }
    let position = 0;
    if (options.mode === "APPENDING") {
      position = await opened.size();
    }
    const at = numberOf(options.position);
    if (at !== undefined) {
      position = at;
    }
    this.context.datasets[name] = {handle: opened, mode: options.mode, binary: options.binary, position};
    setSubrc(0);
  }

  public async closeDataset(nameIn: ICharacter | FieldSymbol | string): Promise<void> {
    this.host("CLOSE DATASET");
    const name = nameOf(nameIn);
    const file = this.context.datasets[name];
    // closing a file that is not open is no error (measured: sy-subrc 0)
    if (file !== undefined) {
      delete this.context.datasets[name];
      await file.handle.close();
    }
    setSubrc(0);
  }

  public async deleteDataset(nameIn: ICharacter | FieldSymbol | string): Promise<void> {
    const host = this.host("DELETE DATASET");
    const name = nameOf(nameIn);
    // deleting an open file answers 0 and closes it: a READ after it raises
    // CX_SY_FILE_OPEN_MODE (measured)
    const file = this.context.datasets[name];
    if (file !== undefined) {
      delete this.context.datasets[name];
      await file.handle.close();
    }
    setSubrc(await host.delete(name) ? 0 : 4);
  }

  public async transfer(sourceIn: any, nameIn: ICharacter | FieldSymbol | string, options: ITransferOptions = {}): Promise<void> {
    this.host("TRANSFER");
    const file = this.opened(nameOf(nameIn));
    if (file.mode === "INPUT") {
      throwError("CX_SY_FILE_OPEN_MODE");
    }
    const source = deref(sourceIn);
    const length = numberOf(options.length);
    let bytes: Uint8Array;
    if (file.binary === true) {
      if (isHex(source) || source instanceof XString) {
        bytes = hexToBytes(source.get());
      } else if (isCharLike(source)) {
        let text: string = source.get();
        if (source instanceof Character) {
          text = text.padEnd(source.getLength(), " ");
        }
        bytes = utf16le(length === undefined ? text : text.substring(0, length));
        if (length !== undefined) {
          const want = length * 2;
          bytes = bytes.length >= want ? bytes : concat(bytes, utf16le(" ".repeat((want - bytes.length) / 2)));
        }
      } else {
        throw new Error("TRANSFER, BINARY MODE supports byte-like and character-like fields only");
      }
      if (length !== undefined && (isHex(source) || source instanceof XString)) {
        bytes = bytes.length >= length ? bytes.slice(0, length) : concat(bytes, new Uint8Array(length - bytes.length));
      }
    } else {
      if (!isCharLike(source)) {
        throw new Error("TRANSFER, TEXT MODE is only supported for character-type data objects");
      }
      let text: string = source.get();
      if (length !== undefined) {
        if (source instanceof Character) {
          text = text.padEnd(source.getLength(), " ");
        }
        text = text.substring(0, length);
      } else if (source instanceof Character) {
        // measured: c(10) 'ab' is written as 'ab' and a line end;
        // a string keeps its trailing blanks
        text = source.getTrimEnd();
      }
      if (options.noEndOfLine !== true) {
        text = text + "\n";
      }
      bytes = new TextEncoder().encode(text);
    }
    await file.handle.write(file.position, bytes);
    file.position += bytes.length;
    setSubrc(0);
  }

  public async readDataset(nameIn: ICharacter | FieldSymbol | string, targetIn: any, options: IReadDatasetOptions = {}): Promise<void> {
    this.host("READ DATASET");
    const file = this.opened(nameOf(nameIn));
    const target = deref(targetIn);
    const maximum = numberOf(options.maximumLength);
    const setActual = (n: number) => {
      if (options.actualLength !== undefined) {
        deref(options.actualLength).set(n);
      }
    };
    // a file opened for writing only answers 4 (measured, OUTPUT and APPENDING)
    if (file.mode === "OUTPUT" || file.mode === "APPENDING") {
      setActual(0);
      setSubrc(4);
      return;
    }
    if (file.binary === false) {
      await this.readLine(file, target, maximum, setActual);
    } else {
      await this.readBytes(file, target, maximum, setActual);
    }
  }

  // TEXT MODE: one line per READ, without its LF (a CR before it stays,
  // measured); the last line needs no LF; at the end sy-subrc 4 and the target
  // cleared. ACTUAL LENGTH is the whole line even when the target is shorter,
  // and the whole line is consumed either way.
  private async readLine(file: OpenDataset, target: any, maximum: number | undefined, setActual: (n: number) => void) {
    if (!isCharLike(target)) {
      throw new Error("READ DATASET, the current statement is only supported for character-type data objects");
    }
    if (maximum !== undefined) {
      throw new Error("READ DATASET, MAXIMUM LENGTH in TEXT MODE not supported");
    }
    let line: Uint8Array<ArrayBufferLike> = new Uint8Array(0);
    let position = file.position;
    let ended = false;
    for (;;) {
      const chunk = await file.handle.read(position, CHUNK);
      if (chunk.length === 0) {
        break;
      }
      const lf = chunk.indexOf(0x0A);
      if (lf >= 0) {
        line = concat(line, chunk.subarray(0, lf));
        position += lf + 1;
        ended = true;
        break;
      }
      line = concat(line, chunk);
      position += chunk.length;
    }
    if (ended === false && line.length === 0) {
      target.clear();
      setActual(0);
      setSubrc(4);
      return;
    }
    file.position = position;
    const text = new TextDecoder("utf-8").decode(line);
    target.set(text);
    setActual(text.length);
    setSubrc(0);
  }

  // BINARY MODE: a field of fixed length takes its length in bytes (a C
  // field two per character, UTF-16LE), a string or xstring the rest of the
  // file; MAXIMUM LENGTH caps the bytes; fewer bytes than asked is sy-subrc 4
  // and ACTUAL LENGTH counts bytes (measured)
  private async readBytes(file: OpenDataset, target: any, maximum: number | undefined, setActual: (n: number) => void) {
    let want: number;
    if (isHex(target)) {
      want = target.getLength();
    } else if (target instanceof Character) {
      want = target.getLength() * 2;
    } else if (target instanceof XString || target instanceof String) {
      want = Math.max(0, await file.handle.size() - file.position);
    } else if (isCharLike(target) && typeof target.getLength === "function") {
      want = target.getLength() * 2;
    } else {
      throw new Error("READ DATASET, BINARY MODE supports byte-like and character-like fields only");
    }
    const variable = target instanceof XString || target instanceof String;
    if (maximum !== undefined) {
      want = Math.min(want, maximum);
    }
    const bytes = want === 0 ? new Uint8Array(0) : await file.handle.read(file.position, want);
    file.position += bytes.length;
    if (isHex(target)) {
      target.set(bytesToHex(bytes).padEnd(target.getLength() * 2, "0"));
    } else if (target instanceof XString) {
      target.set(bytesToHex(bytes));
    } else {
      target.set(fromUtf16le(bytes));
    }
    setActual(bytes.length);
    if (variable === true) {
      setSubrc(bytes.length > 0 ? 0 : 4);
    } else {
      setSubrc(bytes.length < want || want === 0 ? 4 : 0);
    }
  }

  public async getDataset(nameIn: ICharacter | FieldSymbol | string, options: IGetDatasetOptions): Promise<void> {
    this.host("GET DATASET");
    const file = this.opened(nameOf(nameIn));
    if (options.attributes !== undefined) {
      throw new Error("GET DATASET, ATTRIBUTES not supported");
    }
    if (options.position !== undefined) {
      deref(options.position).set(file.position);
    }
    setSubrc(0);
  }

  public async setDataset(nameIn: ICharacter | FieldSymbol | string,
                          options: {position?: INumeric | FieldSymbol, endOfFile?: boolean}): Promise<void> {
    this.host("SET DATASET");
    const file = this.opened(nameOf(nameIn));
    if (options.endOfFile === true) {
      file.position = await file.handle.size();
    } else if (options.position !== undefined) {
      file.position = numberOf(options.position) ?? 0;
    }
    setSubrc(0);
  }
}
