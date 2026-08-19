import {Context} from "../context";
import {INumeric} from "../types/_numeric";

export interface ISkipOptions {
  lines?: INumeric,
  toLine?: INumeric,
}

export class SkipStatement {
  private readonly context: Context;

  public constructor(context: Context) {
    this.context = context;
  }

  public skip(options?: ISkipOptions): void {
    let lines = options?.lines?.get() ?? 1;

    if (options?.toLine !== undefined) {
      const currentLine = this.context.console.get().split("\n").length;
      lines = options.toLine.get() - currentLine;
    }

    lines = Math.max(0, Math.trunc(lines));
    if (lines > 0) {
      this.context.console.add("\n".repeat(lines));
    }
  }
}
