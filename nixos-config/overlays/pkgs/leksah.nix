self: super:

let src = self.fetchFromGitHub {
            owner = "leksah";
            repo = "leksah";
            rev = "45b6cb60cefe65bc4e296249ed2adbc5f662bc5c";
            sha256 = "1ky7r0l14pdlmysfpyks4vncwkdyba469qh29jzpyr7zgy0vy6v4";
            fetchSubmodules = true;
          };

in {

  leksah = (import src {}).overrideAttrs(oldAttrs: {
    postInstall = "echo ${src} >$out/prevent-ifd-gc";
  });

}
