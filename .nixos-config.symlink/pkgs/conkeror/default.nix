super: self:

let

  mkExtension = attrs: super.stdenv.mkDerivation (attrs // { # A hack to force rebuilding when attrs change?…
    name = builtins.baseNameOf attrs.url;
    src = super.fetchurl { inherit (attrs) url sha256; };
    buildInputs = with super; [ unzip zip ];
    unpackCmd = "unzip -d out $curSrc"; # I can’t use `fetchzip`, ’cause it doesn’t know `.xpi`s.
    installPhase = ''
      rm -r META-INF || true

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
  });

  ublock = mkExtension {
    id = "uBlock0@raymondhill.net";
    url = "https://github.com/gorhill/uBlock/releases/download/1.7.4/uBlock0.firefox.xpi";
    sha256 = "13av7d9ayps9n0fas672agqpl1fy3ad73nc4kbfxab97s6jqv90q";
  };

  httpsEverywhere = mkExtension {
    id = "https-everywhere-eff@eff.org";
    url = "https://www.eff.org/files/https-everywhere-5.1.10-eff.xpi";
    sha256 = "01rzprklbg13rjdgyg4q4y21f08g49ikc92m68j9iwrda5fs3ncq";
  };

  domInspector = mkExtension {
    id = "inspector@mozilla.org";
    url = "https://addons.mozilla.org/firefox/downloads/file/324966/dom_inspector-2.0.16-sm+fn+tb+fx.xpi";
    sha256 = "09l1waqm7f3hgbkhb9cznh03ilxkzp8g1a8akxz9aplkbnikslvf";
  };

  noscript = mkExtension {
    id = "{73a6fe31-595d-460b-a920-fcc0f8843232}";
    url = "https://secure.informaction.com/download/releases/noscript-2.9.0.11.xpi";
    sha256 = "141k4jwc1varp9rsm1a61aqrva1192jsvlx710dnbb48j6k6bngx";
  };

  mozrepl = mkExtension {
    id = "mozrepl@hyperstruct.net";
    url = "https://addons.mozilla.org/firefox/downloads/file/205776/mozrepl-1.1.2-fx.xpi";
    sha256 = "09f8mqdcng0rc0ljhyb4n28j6268y0gxnyir1l7sh1ch5jg27szl";
  };

  allExtensions = [
    ublock httpsEverywhere domInspector noscript mozrepl
  ];

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
    ${super.lib.concatMapStrings (ext: "ln -s ${ext} $ext/'${ext.id}'.xpi\n") allExtensions}
  '';

})
