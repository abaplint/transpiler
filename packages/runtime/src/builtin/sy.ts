import {Structure, Integer, Character, Numc, Date, Time} from "../types";

export const sy = new Structure({
  abcde: new Character({length: 26}).set("ABCDEFGHIJKLMNOPQRSTUVWXYZ"),
  datlo: new Date(),
  datum: new Date(),
  index: new Integer(),
  langu: new Character({length: 1}).set("E"),
  mandt: new Character({length: 3}).set("123"),
  msgid: new Character({length: 20}),
  msgno: new Numc({length: 3}),
  msgv1: new Character({length: 50}),
  msgv2: new Character({length: 50}),
  msgv3: new Character({length: 50}),
  msgv4: new Character({length: 50}),
  subrc: new Integer(),
  sysid: new Character({length: 3}).set("ABC"),
  tabix: new Integer(),
  timlo: new Time(),
  uname: new Character({length: 12}).set("USERNAME"),
  uzeit: new Time(),
});