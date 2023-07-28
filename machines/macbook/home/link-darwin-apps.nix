{
  config,
  pkgs,
  lib,
  ...
}: let
  appsToLink =
    __concatMap (
      package: let
        apps = "${package}/Applications";
      in
        if __pathExists apps
        then map (app: "${apps}/${app}") (__attrNames (__readDir apps))
        else []
    )
    config.home.packages;
in {
  home.file = __listToAttrs (map (app: {
      name = "Applications/${__unsafeDiscardStringContext (baseNameOf app)}/Contents";
      value.source = "${app}/Contents";
    })
    appsToLink);
}
