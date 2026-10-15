import {DecFloat34} from "../types";
import {ticksToSeconds, utclongToTicks, UTCLongInput} from "./_utclong";

export interface IUTCLongDiffInput {
  high: UTCLongInput,
  low: UTCLongInput,
}

/** high minus low in seconds, exact to seven decimals. An initial value counts as the lowest time stamp. */
export function utclong_diff(input: IUTCLongDiffInput): DecFloat34 {
  return ticksToSeconds(utclongToTicks(input.high) - utclongToTicks(input.low));
}
