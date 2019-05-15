{ config, lib, pkgs, ... }:

let

  base = pkgs.emacs25.override {
    # Use ‘lucid’ toolkit—it doesn’t have this bug → https://bugzilla.gnome.org/show_bug.cgi?id=85715
    withX = true;
    withGTK2 = false;
    withGTK3 = false;
  };

  whole = (pkgs.emacsPackagesNgGen base).emacsWithPackages (epkgs:
    # MELPA Unstable @ NixOS Unstable (bleedingest edge)
    (with (pkgs.nixos-unstable.emacsPackagesNgGen base).melpaPackages; [
      company-lsp
      dhall-mode
      focus-autosave-mode
      google-translate
      hayoo
      lsp-haskell
      lsp-mode
      lsp-ui
      nix-mode
      python-mode
      sort-words
      super-save
    ])
    ++

    # ELPA Stable
    (with epkgs.elpaPackages; [
      auctex
      nlinum
    ])
    ++

    # MELPA Stable
    (with epkgs.melpaStablePackages; [
      #gregorio-mode
      company
      counsel
      diff-hl
      ensime
      erlang
      expand-region
      flycheck
      git-gutter
      git-link
      go-mode
      haskell-mode
      hindent
      hl-todo
      ivy
      magit
      markdown-mode
      move-text
      neotree
      projectile
      scala-mode
      smart-mode-line
      solarized-theme
      swiper
      use-package
      ws-butler
      yaml-mode
    ])
  );

in

{
  environment.systemPackages = [ whole ] ++ (with pkgs; [
    dhall
  ]);

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
