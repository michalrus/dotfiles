{config, ...}: let
  appsToLink =
    builtins.concatMap (
      package: let
        apps = "${package}/Applications";
      in
        if builtins.pathExists apps
        then builtins.map (app: "${apps}/${app}") (builtins.attrNames (builtins.readDir apps))
        else []
    )
    config.home.packages;
in {
  home.file = builtins.listToAttrs (builtins.map (app: {
      name = "Applications/${builtins.unsafeDiscardStringContext (builtins.baseNameOf app)}/Contents";
      value.source = "${app}/Contents";
    })
    appsToLink);
}
