import {UTCLong} from "../types";
import {currentTicks, ticksToUTCLong} from "./_utclong";

/** The current UTC time stamp, with the millisecond resolution of the JavaScript clock. */
export function utclong_current(): UTCLong {
  return ticksToUTCLong(currentTicks());
}
