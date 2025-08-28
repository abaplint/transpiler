import {DataReference, HashedTable, Table} from "./types";

export async function parametersCall(method: (params: any) => Promise<any>, parameters: Table | HashedTable): Promise<void> {
  const input: Record<string, any> = {};
  let receiving: DataReference | undefined = undefined;

  for (const v of parameters.array()) {
    const kind = v.get().kind.get();
    if (kind === "") {
      throw new Error("open-abap currently requires KIND to be set in PARAMETER-TABLE calls");
    } else if (kind === "R") {
      receiving = v.get().value;
      continue;
    }

    const name = v.get().name.get().toLowerCase().trimEnd();
    const value = v.get().value.dereference();
    input[name] = value;
  }

  const res = await method(input);
  if (res !== undefined) {
    receiving?.assign(res);
  }

  console.dir(res);
  console.dir(receiving);
}