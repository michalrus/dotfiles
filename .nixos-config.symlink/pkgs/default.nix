{

  nixpkgs.config.packageOverrides = super: let self = super.pkgs; in {
    awf-gtk            = (import ./awf-gtk.nix super self);
    imgurbash2         = (import ./imgurbash2.nix super self);
    conkeror-unwrapped = (import ./conkeror-unwrapped.nix super self);
    conky              = (import ./conky super self);
    mtr                = (import ./mtr.nix super self);
    mu                 = (import ./mu super self);
    st                 = (import ./st super self);
    transcribe         = (import ./transcribe.nix super self);
    visualvm           = (import ./visualvm.nix super self);
  };

}
