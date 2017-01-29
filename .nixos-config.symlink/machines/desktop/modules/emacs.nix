{ config, lib, pkgs, ... }:

let

  # FIXME: change it to 25! Currently some packages stopped compiling.
  customBuild = pkgs.emacs25.override {
    # Use ‘lucid’ toolkit. Doesn’t have this bug → https://bugzilla.gnome.org/show_bug.cgi?id=85715
    withX = true;
    withGTK2 = false;
    withGTK3 = false;
  };

  custom = (pkgs.emacsPackagesNgGen customBuild).override (super: self: {
    # Use these from MELPA Unstable:
    inherit (self.melpaPackages) intero;

    # FIXME: use it from nixpkgs when regenerated
    frames-only-mode = self.melpaBuild {
      pname = "frames-only-mode";
      version = "20170129.120";
      src = pkgs.fetchFromGitHub {
        owner = "davidshepherd7";
        repo = "frames-only-mode";
        rev = "5a2947d797a5d6f74d3a9c97f8c0ab6cff115b28";
        sha256 = "0y0sdjixaxvywrlp2sw51wnczhk51q1svl5aghbk9rkxpwv9ys9v";
      };
      recipeFile = pkgs.fetchurl {
        url = "https://raw.githubusercontent.com/melpa/melpa/63889df90a8cd4a39871cc43ccc559eff7b8dd5f/recipes/frames-only-mode";
        sha256 = "17p04l16ghz9kk096xk37yjpi4rmla86gp7c8ysjf6q6nyh0608h";
        name = "frames-only-mode";
      };
    };
  });

  customEmacs = custom.emacsWithPackages (epkgs: with epkgs; [
    auctex
    bbdb
    company
    counsel
    diff-hl
    ensime
    expand-region
    frames-only-mode
    git-link
    go-mode
    gregorio-mode
    haskell-mode
    hindent
    intero
    ivy
    magit
    markdown-mode
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
