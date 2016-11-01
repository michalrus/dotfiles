{ config, lib, pkgs, ... }:

let

  custom = (pkgs.emacsPackagesNgGen pkgs.emacs25).override (super: self: {
    inherit (self.melpaPackages)
      # Use these from MELPA Unstable:
      ivy swiper counsel;
  });

in

{
  environment.systemPackages = [
    (custom.emacsWithPackages (epkgs: with epkgs; [
      auctex
      bbdb
      company
      counsel
      diff-hl
      ensime
      expand-region
      go-mode
      gregorio-mode
      haskell-mode
      hindent
      intero
      magit
      neotree
      projectile
      python-mode
      sane-term
      scala-mode
      solarized-theme
      swiper
      use-package
      yaml-mode
      zoom-frm
    ]))
  ];
}
