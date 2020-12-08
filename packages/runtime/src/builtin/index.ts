import {Character} from "../types";

export * from "./concat_lines_of";
export * from "./count";
export * from "./condense";
export * from "./lines";
export * from "./strlen";
export * from "./substring";
export * from "./sy";
export * from "./boolc";
export * from "./reverse";
export * from "./xstrlen";
export const abap_true = new Character({length: 1}).set("X");
export const abap_false = new Character({length: 1}).set("");
export const abap_undefined = new Character({length: 1}).set("-");
export const space = new Character({length: 1}).set(" ");