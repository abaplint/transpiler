import {DatasetHandle, DatasetHost, DatasetMode} from "./dataset";

/** A DatasetHost over a map of names to bytes: for tests, and for a runtime
 *  in a browser, where there is no file system to hand to OPEN DATASET.
 *  OUTPUT empties or creates a file, APPENDING and UPDATE create it when
 *  missing, INPUT of a missing file fails with the message a system gives. */
export class MemoryDataset implements DatasetHost {
  public readonly files: {[name: string]: Uint8Array};

  public constructor(files: {[name: string]: Uint8Array} = {}) {
    this.files = files;
  }

  public async open(name: string, mode: DatasetMode): Promise<DatasetHandle | {message: string}> {
    const files = this.files;
    if (files[name] === undefined) {
      if (mode === "INPUT") {
        return {message: "No such file or directory"};
      }
      files[name] = new Uint8Array(0);
    }
    if (mode === "OUTPUT") {
      files[name] = new Uint8Array(0);
    }
    return {
      read: async (position: number, length: number) => files[name].slice(position, position + length),
      write: async (position: number, bytes: Uint8Array) => {
        const end = Math.max(files[name].length, position + bytes.length);
        const out = new Uint8Array(end);
        out.set(files[name], 0);
        out.set(bytes, position);
        files[name] = out;
      },
      size: async () => files[name].length,
      close: async () => undefined,
    };
  }

  public async delete(name: string): Promise<boolean> {
    if (this.files[name] === undefined) {
      return false;
    }
    delete this.files[name];
    return true;
  }
}
