{ config, lib, pkgs, ... }:

let

  customBuild = pkgs.emacs25.override {
    # Use ‘lucid’ toolkit. Doesn’t have this bug → https://bugzilla.gnome.org/show_bug.cgi?id=85715
    withX = true;
    withGTK2 = false;
    withGTK3 = false;
  };

  custom = (pkgs.emacsPackagesNgGen customBuild).override (super: self: {
    inherit (self.melpaPackages)
    # Use these from MELPA Unstable:
    intero;
  });

  customEmacs = custom.emacsWithPackages (epkgs: with epkgs; [
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
    ivy
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
  ]);

in

{
  environment.systemPackages = [ customEmacs pkgs.screen ];

  systemd.user.services.emacs-daemon = {
    description = "Emacs: the extensible, self-documenting text editor";
    serviceConfig = {
      Type = "forking";
      Restart = "always";
      ExecStop = ''${customEmacs}/bin/emacsclient --eval "(progn (setq kill-emacs-hook 'nil) (kill-emacs))"'';
    };
    script = ''
      # Remove the desktop.lock in case the last exit was unclean.
      rm "$HOME"/.emacs.d/.emacs.desktop.lock 2>/dev/null || true

      # Read login shell variables set in ~/.profile or similar.
      exec "$SHELL" --login -c "exec ${customEmacs}/bin/emacs --daemon"
    '';
  };
}
