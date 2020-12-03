import {Structure, Integer, Character} from "../types";

export const sy = new Structure({
  mandt: new Character({length: 3}).set("123"),
  index: new Integer(),
  tabix: new Integer(),
  subrc: new Integer(),
});