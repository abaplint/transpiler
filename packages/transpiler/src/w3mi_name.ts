import * as abaplint from "@abaplint/core";

/**
 * The name a system knows a Web Repository object by: `wwwdata-objid`.
 *
 * It is not the file name. abapGit percent-escapes the characters a file name
 * cannot hold, so `ZOISEE-EAR-02.MP3` is stored as `zoisee-ear-02%2emp3`, and
 * abaplint derives an object's name from the file it was read from. Keying
 * anything a caller sees on that spelling turns an abapGit storage detail into
 * an API: ABAP that asks `SELECT ... FROM wwwparams WHERE objid = 'X.MP3'`,
 * which is how the object is addressed on a system, then finds nothing.
 *
 * The real name is in the object's own XML, and abapGit puts it there from the
 * key itself — `io_xml->add( iv_name = 'NAME' ig_data = ms_key-objid )` in
 * `zcl_abapgit_object_w3xx_super`, in both serializer versions.
 */
export function w3miObjectName(obj: abaplint.Objects.WebMIME): string {
  const raw = obj.getXMLFile()?.getRaw();
  if (raw !== undefined) {
    // every entry of <PARAMS> carries a <NAME> of its own, so only the part
    // before the parameter list can be searched for the object's
    const head = raw.split(/<PARAMS[\s/>]/)[0];
    const found = /<NAME>([^<]*)<\/NAME>/.exec(head);
    const name = found === null ? "" : unescapeXML(found[1]).trim();
    if (name.length > 0) {
      return name.toUpperCase();
    }
  }
  // an object with no XML, or an XML from something that is not abapGit: the
  // file name is the only name there is, and a wrong key beats no key
  return obj.getName().toUpperCase();
}

function unescapeXML(value: string): string {
  // a W3MI name may hold < and > and &, which reach the file escaped
  return value.replace(/&lt;/g, "<")
    .replace(/&gt;/g, ">")
    .replace(/&quot;/g, "\"")
    .replace(/&apos;/g, "'")
    .replace(/&amp;/g, "&");
}
