import {throwError} from "../throw_error";
import {UTCLong} from "../types";
import {isInRange, NumberInput, secondsToTicks, ticksToUTCLong, TICKS, utclongToTicks, UTCLongInput, wholeUnits} from "./_utclong";

export interface IUTCLongAddInput {
  val: UTCLongInput,
  days?: NumberInput,
  hours?: NumberInput,
  /** abaplint up to now declares the parameter as "hour" */
  hour?: NumberInput,
  minutes?: NumberInput,
  seconds?: NumberInput,
}

/** Days, hours and minutes are added first, then the seconds, rounded to seven decimals.
 *  An initial val counts as the lowest time stamp, and the result is never initial. */
export function utclong_add(input: IUTCLongAddInput): UTCLong {
  let ticks = utclongToTicks(input.val);

  ticks += wholeUnits(input.days) * TICKS.day
    + wholeUnits(input.hours ?? input.hour) * TICKS.hour
    + wholeUnits(input.minutes) * TICKS.minute;
  if (isInRange(ticks) === false) {
    throwError("CX_SY_ARITHMETIC_OVERFLOW");
  }

  ticks += secondsToTicks(input.seconds);
  if (isInRange(ticks) === false) {
    throwError("CX_SY_ARITHMETIC_OVERFLOW");
  }

  return ticksToUTCLong(ticks);
}
