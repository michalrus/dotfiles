{ pkgs }:

let

  iVersion = "0.1.23";

  iSrc = pkgs.fetchFromGitHub {
    owner = "commercialhaskell";
    repo = "intero";
    rev = iVersion;
    sha256 = "1q6q2hnqf78kxd61nic4zjx7crbv8p25p4aq0h4vihamm8r0v7vm";
  };

  iHaskellPackages = pkgs.haskellPackages.override {
    overrides = self: super: {
      intero = pkgs.haskell.lib.overrideCabal (self.callCabal2nix "intero" iSrc {}) (drv: {
        preCheck = ''
          export PATH="$PWD/dist/build/intero:$PATH"
        '';
      });
    };
  };

in rec {

  haskellPackages = iHaskellPackages;

  inherit (iHaskellPackages) intero;

  nix-shim = import (pkgs.fetchFromGitHub {
    owner = "michalrus";
    repo = "intero-nix-shim";
    rev = "ee504e490093567c4b1c122dc45358419d451fb5";
    sha256 = "07vsqr9cf6n2r7zzb5w3dy78adii9ghknz7476lhlgfv4g6nwx74";
  }) {
    nixpkgs = pkgs;
    haskellPackages = iHaskellPackages;
  };

  emacsMode = emacs: emacs.melpaBuild {
    pname = "intero";
    src = "${iSrc}/elisp";
    version = iVersion;
    packageRequires = with emacs; [ company flycheck haskell-mode ];
    propagatedUserEnvPkgs = [ nix-shim ];
  };

}
