{

  nixpkgs.config.packageOverrides = super: let self = super.pkgs; in {
    awf-gtk            = (import ./awf-gtk.nix super self);
    conkeror-unwrapped = (import ./conkeror super self);
    conky              = (import ./conky super self);
    evince             = (import ./evince.nix super self);
    gregorio           = (import ./gregorio.nix super self);
    influxdb           = (import ./influxdb super self);
    mtr                = (import ./mtr.nix super self);
    mu                 = (import ./mu super self);
    st                 = (import ./st super self);
    tcp-broadcast      = (import ./tcp-broadcast.nix super self);
    transcribe         = (import ./transcribe.nix super self);
    visualvm           = (import ./visualvm.nix super self);
  };

}
