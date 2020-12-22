import {Structure, Integer, Character, Numc} from "../types";

export const sy = new Structure({
  mandt: new Character({length: 3}).set("123"),
  index: new Integer(),
  tabix: new Integer(),
  subrc: new Integer(),
  msgid: new Character({length: 20}),
  msgno: new Numc({length: 3}),
  msgv1: new Character({length: 50}),
  msgv2: new Character({length: 50}),
  msgv3: new Character({length: 50}),
  msgv4: new Character({length: 50}),
});