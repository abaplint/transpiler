import {Structure} from "../types";

export function moveCorresponding(source: Structure, target: Structure): void {
  for (const n in source.get()) {
    target.get()[n]?.set(source.get()[n]);
  }
}