import {HashedTable, Table} from "./types";

export async function parametersCall(method: (params: any) => Promise<any>, parameters: Table | HashedTable): Promise<void> {
  const input: Record<string, any> = {};

  for (const v of parameters.array()) {
    const name = v.get().name.get().toLowerCase().trimEnd();
    const kind = v.get().kind.get();
    if (kind === "") {
      throw new Error("open-abap currently requires KIND to be set in PARAMETER-TABLE calls");
    }
    const value = v.get().value.dereference();
    input[name] = value;
  }

  await method(input);
}