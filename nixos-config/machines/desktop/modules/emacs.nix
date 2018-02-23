{ config, lib, pkgs, ... }:

let

  base = pkgs.nixos-unstable.emacs25.override {
    # Use ‘lucid’ toolkit—it doesn’t have this bug → https://bugzilla.gnome.org/show_bug.cgi?id=85715
    withX = true;
    withGTK2 = false;
    withGTK3 = false;
  };

  whole = (pkgs.nixos-unstable.emacsPackagesNgGen base).emacsWithPackages (epkgs:
    # MELPA Unstable
    (with epkgs.melpaPackages; [
      ensime
      hayoo
      lsp-mode
      lsp-ui
      lsp-haskell
      company-lsp
      sort-words
      use-package
    ])
    ++

    # ELPA Stable
    (with epkgs.elpaPackages; [
      auctex
    ])
    ++

    # MELPA Stable
    (with epkgs.melpaStablePackages; [
      company
      counsel
      diff-hl
      expand-region
      flycheck
      git-link
      go-mode
      google-translate
      #gregorio-mode
      haskell-mode
      hindent
      hl-todo
      ivy
      magit
      markdown-mode
      neotree
      projectile
      python-mode
      scala-mode
      solarized-theme
      swiper
      yaml-mode
    ])
  );

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
