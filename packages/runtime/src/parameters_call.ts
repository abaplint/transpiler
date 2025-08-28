import {HashedTable, Table} from "./types";

export async function parametersCall(method: (params: any) => Promise<any>, parameters: Table | HashedTable): Promise<void> {
  const input = parameters.array().reduce((a, v) => ({...a, [v.get().name.get(
          ).toLowerCase().trimEnd()]: v.get().value.dereference()}), {})

  await method(input);
}