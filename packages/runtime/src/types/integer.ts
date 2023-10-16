/* eslint-disable default-case */
import {throwError} from "../throw_error";
import {Float} from "./float";
import {Hex} from "./hex";
import {XString} from "./xstring";
import {ICharacter} from "./_character";
import {INumeric} from "./_numeric";

const digits = new RegExp(/^\s*-?\+?\d+\.?\d* *$/i);

export class Integer implements INumeric {
  private value: number;
  private constant: boolean = false;
  private readonly qualifiedName: string | undefined;

  public constructor(input?: {qualifiedName?: string}) {
    this.value = 0;
    this.qualifiedName = input?.qualifiedName;
  }

  public getQualifiedName() {
    return this.qualifiedName;
  }

  public setConstant() {
    this.constant = true;
    return this;
  }

  public set(value: INumeric | ICharacter | Hex | string | number | Integer | Float) {
    if (this.constant === true) {
      throw new Error("Changing constant");
    }

    switch (typeof value) {
      case "number":
        this.value = Math.round(value);
        return this;
      case "string":
        if (value.endsWith("-")) {
          value = "-" + value.substring(0, value.length - 1);
        }
        if (value.trim().length === 0) {
          value = "0";
        } else if (digits.test(value) === false) {
          throwError("CX_SY_CONVERSION_NO_NUMBER");
        }
        this.value = parseInt(value, 10);
        return this;
      case "object":
        switch (value.constructor) {
          case Float:
            this.set(Math.round((value as Float).getRaw()));
            return this;
          case Hex:
          case XString:
            {
              let num = parseInt((value as Hex | XString).get(), 16);
              // handle two complement,
              if (value instanceof Hex && value.getLength() >= 4) {
                const maxVal = Math.pow(2, value.get().length / 2 * 8);
                if (num > maxVal / 2 - 1) {
                  num = num - maxVal;
                }
              }
              this.set(num);
            }
            return this;
        }
    }

    this.set(value.get());
    return this;
  }

  public clear(): void {
    this.value = 0;
  }

  public get(): number {
    return this.value;
  }
}