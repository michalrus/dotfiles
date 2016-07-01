super: self:

let

  ublock =  { extName = "uBlock0@raymondhill.net"; } // super.fetchurl {
    url = "https://github.com/gorhill/uBlock/releases/download/1.7.4/uBlock0.firefox.xpi";
    sha256 = "13av7d9ayps9n0fas672agqpl1fy3ad73nc4kbfxab97s6jqv90q";
  };

  httpsEverywhere =  { extName = "https-everywhere-eff@eff.org"; } // super.fetchurl {
    url = "https://www.eff.org/files/https-everywhere-5.1.10-eff.xpi";
    sha256 = "01rzprklbg13rjdgyg4q4y21f08g49ikc92m68j9iwrda5fs3ncq";
  };

  noscript = super.stdenv.mkDerivation {
    name = "noscript";
    extName = "{73a6fe31-595d-460b-a920-fcc0f8843232}"; # <em:id/> from install.rdf
    src = super.fetchurl {
      url = "https://secure.informaction.com/download/releases/noscript-2.9.0.11.xpi";
      sha256 = "141k4jwc1varp9rsm1a61aqrva1192jsvlx710dnbb48j6k6bngx";
    };
    buildInputs = with super; [ unzip zip ];
    unpackCmd = "unzip -d out $curSrc";
    installPhase = ''
      rm -r META-INF

      printf "" >install.rdf.new
      sed '/<\/em:targetApplication>/q' install.rdf >>install.rdf.new
      cat <<'EOF' >>install.rdf.new
         <!-- Conkeror -->
         <em:targetApplication>
           <Description>
             <em:id>{a79fe89b-6662-4ff4-8e88-09950ad4dfde}</em:id>
             <em:minVersion>0.1</em:minVersion>
             <em:maxVersion>9.9</em:maxVersion>
           </Description>
         </em:targetApplication>
      EOF
      sed '1,/<\/em:targetApplication>/d' install.rdf >>install.rdf.new
      mv install.rdf.new install.rdf

      zip -r out.zip . ; mv out.zip $out
      '';
  };

in

super.conkeror-unwrapped.overrideDerivation (oldAttrs: {

  name = "conkeror-1.0.3";

  src = super.fetchgit {
    url = git://repo.or.cz/conkeror.git;
    rev = "6d4599333c402f1dcdfdb60812068060e30a10d9";
    sha256 = "1vqyv9ga1ksqq1mkn92r0wbxgpbpd0cq46hfrfknaik0nzca7pq2";
  };

  patches = [ ./ctrl-click-in-new-buffer.patch ];

  installPhase = oldAttrs.installPhase + ''
    ext=$out/libexec/conkeror/extensions
    mkdir -p $ext
    ln -s ${ublock} $ext/'${ublock.extName}.xpi'
    ln -s ${httpsEverywhere} $ext/'${httpsEverywhere.extName}.xpi'
    ln -s ${noscript} $ext/'${noscript.extName}.xpi'
    '';

})
