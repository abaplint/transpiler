import * as types from "./types";
import * as builtin from "./builtin";
import * as compare from "./compare";
import * as statements from "./statements";
import {Console} from "./console";
import {UnitTestResult} from "./unit_test";
import {OffsetLength} from "./offset_length";

const FunctionModules = {};

export {
  types, statements, builtin, compare,
  FunctionModules,
  Console, UnitTestResult, OffsetLength};