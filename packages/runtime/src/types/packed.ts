import {Float} from "./float";
import {INumeric} from "./_numeric";
import {throwError} from "../throw_error";
import {Integer8} from "./integer8";

const digits = new RegExp(/^\s*-?\+?\d*\.?\d* *$/i);

export class Packed implements INumeric {
  // todo: change this to bigint to get the proper precision? for larger than maxsafeint
  private value: number;
  private readonly length: number;
  private readonly decimals: number;
  private readonly qualifiedName: string | undefined;

  public constructor(input?: {length?: number, decimals?: number, qualifiedName?: string}) {
    this.value = 0;

    this.length = 666;
    if (input?.length) {
      this.length = input.length;
    }

    this.decimals = 0;
    if (input?.decimals) {
      this.decimals = input.decimals;
    }

    this.qualifiedName = input?.qualifiedName;
  }

  public getQualifiedName() {
    return this.qualifiedName;
  }

  private round(value: number, places: number) {
    // @ts-ignore
    return +(Math.round(value + "e+" + places)  + "e-" + places);
  }

  public set(value: INumeric | number | string) {
    if (typeof value === "number") {
      this.value = value;
    } else if (typeof value === "string") {
      if (value.trim().length === 0) {
        this.value = 0;
        return this;
      } else if (digits.test(value) === false) {
        throwError("CX_SY_CONVERSION_NO_NUMBER");
      }

      this.value = this.round(parseFloat(value), this.decimals);
    } else if (value instanceof Integer8) {
      this.value = Number(value.get());
    } else if (value instanceof Float) {
      this.value = this.round(value.getRaw(), this.decimals);
    } else {
      this.set(value.get());
    }
    return this;
  }

  public getLength() {
    return this.length;
  }

  public getDecimals() {
    return this.decimals;
  }

  public clear(): void {
    this.value = 0;
  }

  public get(): number {
    return this.value;
  }
}