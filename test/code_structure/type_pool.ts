import {expect} from "chai";
import {compileFiles} from "../_utils";

describe("Running code structure - Type Pool", () => {

  it("basic", async () => {
    const code = `TYPE-POOL seoo.
CONSTANTS seoo_cmptype_attribute TYPE n LENGTH 1 VALUE '5'.
TYPES seoo_sdf TYPE c LENGTH 1.`;
    const output = await compileFiles([
      {filename: "seoo.type.abap", contents: code},
    ]);
    expect(output.objects.length).to.equal(1);

    expect(output.objects[0].chunk.getCode()).to.equal(`const pool = {};
pool['seoo_cmptype_attribute'] = new abap.types.Numc();
pool['seoo_cmptype_attribute'].set('5');
pool['seoo_sdf'] = new abap.types.Character(1, {"qualifiedName":"seoo_sdf"});
abap.TypePools['SEOO'] = pool;`);
  });

  it.only("structured constant", async () => {
    const code = `TYPE-POOL seoo.
CONSTANTS: BEGIN OF seoo_moo,
             foo TYPE n LENGTH 1 VALUE '5',
           END OF seoo_moo.`;
    const output = await compileFiles([
      {filename: "seoo.type.abap", contents: code},
    ]);
    expect(output.objects.length).to.equal(1);

    expect(output.objects[0].chunk.getCode()).to.equal(`const pool = {};
pool['seoo_cmptype_attribute'] = new abap.types.Numc();
pool['seoo_cmptype_attribute'].set('5');
pool['seoo_sdf'] = new abap.types.Character(1, {"qualifiedName":"seoo_sdf"});
abap.TypePools['SEOO'] = pool;`);
  });

});