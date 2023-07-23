{ inputs }:

let inherit (inputs.nixpkgs) lib; in

rec {

  # Filters a list/attrs of derivations to only include those available on a ‘system’:
  filterSystem =
    system: drvs:
    let p = drv: (!(drv ? meta) || !(drv.meta ? platforms) || lib.elem system drv.meta.platforms); in
    if lib.isAttrs drvs then lib.filterAttrs (_: p) drvs
    else if lib.isList drvs then lib.filter p drvs
    else throw "‘lib.filterSystem’ expects attrs or a list";

}
