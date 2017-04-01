{ config, lib, pkgs, ... }:

let

  emacs = pkgs.emacs25.override {
    # Use ‘lucid’ toolkit—it doesn’t have this bug → https://bugzilla.gnome.org/show_bug.cgi?id=85715
    withX = true;
    withGTK2 = false;
    withGTK3 = false;
  };

  unstable = pkgs.unstable-emacsPackagesNgGen emacs;

  packages = (pkgs.emacsPackagesNgGen emacs).override (super: self: {
    # From MELPA Unstable @ NixOS Stable:
    #inherit (self.melpaPackages) ???;

    # From MELPA Unstable @ NixOS Unstable (bleeding edge):
    inherit (unstable.melpaPackages) intero ensime;
  });

  whole = packages.emacsWithPackages (epkgs: with epkgs; [
    auctex
    bbdb
    company
    counsel
    diff-hl
    ensime
    expand-region
    flycheck
    git-link
    go-mode
    google-translate
    gregorio-mode
    haskell-mode
    hayoo
    hindent
    intero
    ivy
    magit
    markdown-mode
    neotree
    projectile
    python-mode
    scala-mode
    solarized-theme
    swiper
    use-package
    yaml-mode
  ]);

in

{
  environment.systemPackages = [ whole ];

  systemd.user.services.emacs-daemon = {
    description = "Emacs: the extensible, self-documenting text editor";
    serviceConfig = {
      Type = "forking";
      Restart = "always";
      ExecStop = ''${whole}/bin/emacsclient --eval "(progn (setq kill-emacs-hook 'nil) (kill-emacs))"'';
    };
    script = ''
      # Remove the desktop.lock in case the last exit was unclean.
      rm "$HOME"/.emacs.d/.emacs.desktop.lock 2>/dev/null || true

      # Read login shell variables set in ~/.profile or similar.
      exec "$SHELL" --login -c "exec ${whole}/bin/emacs --daemon"
    '';
  };
}
