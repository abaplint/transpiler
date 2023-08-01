
export interface IReceiveOptions {
  exporting?: any,
  importing?: any,
  tables?: any,
  changing?: any,
  exceptions?: any,
}

let cache: IReceiveOptions | undefined = undefined;

export function _receiveSetResult(options: IReceiveOptions) {
  cache = options;
}

export function receive(options: IReceiveOptions) {
  if (cache === undefined) {
    throw new Error("runtime receive(), no results");
  }

  for (const a in cache.importing || []) {
    const aval = cache.importing[a];
    const bval = options.importing[a];
    if (bval === undefined) {
      continue;
    }
    bval.set(aval);
  }

  for (const a in cache.tables || []) {
    const aval = cache.tables[a];
    const bval = options.tables[a];
    if (bval === undefined) {
      continue;
    }
    bval.set(aval);
  }

  for (const a in cache.changing || []) {
    const aval = cache.changing[a];
    const bval = options.changing[a];
    if (bval === undefined) {
      continue;
    }
    bval.set(aval);
  }

  cache = undefined;
}