import {Structure, Integer, Character, Numc, Date, Time} from "../types";

export const sy = new Structure({
  abcde: new Character(26).set("ABCDEFGHIJKLMNOPQRSTUVWXYZ"),
  datlo: new Date(),
  datum: new Date(),
  dbcnt: new Integer(),
  fdpos: new Integer(),
  host: new Character(32).set("localhost"),
  index: new Integer(),
  langu: new Character(1).set("E"),
  mandt: new Character(3).set("123"),
  msgid: new Character(20),
  msgno: new Numc({length: 3}),
  msgty: new Character(1),
  msgv1: new Character(50),
  msgv2: new Character(50),
  msgv3: new Character(50),
  msgv4: new Character(50),
  subrc: new Integer(),
  sysid: new Character(3).set("ABC"),
  tabix: new Integer(),
  tfill: new Integer(),
  timlo: new Time(),
  tzone: new Integer(), // 0 = UTC
  uname: new Character(12).set("USERNAME"),
  uzeit: new Time(),
});