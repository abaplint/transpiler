import {Character} from "../types";

export * from "./abs";
export * from "./boolc";
export * from "./ceil";
export * from "./concat_lines_of";
export * from "./condense";
export * from "./contains";
export * from "./cos";
export * from "./count_any_of";
export * from "./count";
export * from "./escape";
export * from "./find";
export * from "./floor";
export * from "./frac";
export * from "./insert";
export * from "./ipow";
export * from "./lines";
export * from "./match";
export * from "./matches";
export * from "./nmax";
export * from "./nmin";
export * from "./numofchar";
export * from "./repeat";
export * from "./replace";
export * from "./reverse";
export * from "./round";
export * from "./segment";
export * from "./shift_right";
export * from "./shift_left";
export * from "./sign";
export * from "./sin";
export * from "./sqrt";
export * from "./strlen";
export * from "./substring_after";
export * from "./substring_before";
export * from "./substring";
export * from "./sy";
export * from "./tan";
export * from "./to_lower";
export * from "./to_mixed";
export * from "./to_upper";
export * from "./translate";
export * from "./trunc";
export * from "./xstrlen";
export const abap_true = new Character(1, {qualifiedName: "ABAP_BOOL", ddicName: "ABAP_BOOL"}).set("X").setConstant();
export const abap_false = new Character(1, {qualifiedName: "ABAP_BOOL", ddicName: "ABAP_BOOL"}).set("").setConstant();
export const abap_undefined = new Character(1, {qualifiedName: "ABAP_BOOL", ddicName: "ABAP_BOOL"}).set("-").setConstant();
export const space = new Character(1, {qualifiedName: "ABAP_BOOL", ddicName: "ABAP_BOOL"}).set(" ").setConstant();

export const $_backspace = new Character(1).set("\b").setConstant();
export const $_cr_lf = new Character(2).set("\r\n").setConstant();
export const $_formfeed = new Character(1).set("\f").setConstant();
export const $_horizontal_tab = new Character(1).set("\t").setConstant();
export const $_newline = new Character(1).set("\n").setConstant();
export const $_vertical_tab = new Character(1).set("\v").setConstant();
/*
export const $_maxchar = new Character(1).set(Buffer.from("FDFF", "hex").toString()).setConstant();
export const $_minchar = new Character(1).set(Buffer.from("0000", "hex").toString()).setConstant();
*/