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
    rev = "dd852a9f7d830d30875a203df145e9a0b5e40606";
    sha256 = "0jq1hzhx8ax69l4nq58avh0wjyzfychagckla7vx750pgj4jrgd5";
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
