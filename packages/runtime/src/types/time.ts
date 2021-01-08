import {Hex} from "./hex";
import {ICharacter} from "./_character";
import {INumeric} from "./_numeric";

export class Time implements ICharacter {
  private value: string;

  public constructor() {
    this.clear();
  }

  public set(value: INumeric | ICharacter | Hex | string | number) {
    if (typeof value === "number") {
      const date = new Date();
      date.setTime(value * 1000);
      this.value = date.getUTCHours().toString().padStart(2,"0") +
                   date.getUTCMinutes().toString().padStart(2,"0") +
                   date.getUTCSeconds().toString().padStart(2,"0");
    } else if (typeof value === "string") {
      this.value = value;
    } else {
      this.set(value.get());
    }
    return this;
  }

  public clear(): void {
    this.value = "000000";
  }

  public get(): string {
    return this.value;
  }

  public getNumeric(): number {
    const hours = parseInt(this.value.substr(0,2),10);
    const minutes = parseInt(this.value.substr(2,2),10);
    const seconds = parseInt(this.value.substr(4,2),10);
    return hours * 3600 + minutes * 60 + seconds;
  }
}