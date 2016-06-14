super: self:

super.stdenv.mkDerivation rec {
  name = "visualvm-1.3.8";
  src = super.fetchzip {
    url = "https://java.net/projects/visualvm/downloads/download/release138/visualvm_138.zip";
    sha256 = "09wsi85z1g7bwyfhb37vw0gy3wl0j1cy35aj59rg7067q262gy1y";
  };

  installPhase = ''
    rm bin/visualvm.exe

    substituteInPlace etc/visualvm.conf \
      --replace "#visualvm_jdkhome=" "visualvm_jdkhome=" \
      --replace "/path/to/jdk" "${super.openjdk8}/lib/openjdk/" \
      --replace 'visualvm_default_options="' 'visualvm_default_options="--laf com.sun.java.swing.plaf.gtk.GTKLookAndFeel '

    mkdir $out && cp -R . $out/

    wrapProgram $out/bin/visualvm \
      --prefix LD_LIBRARY_PATH : "${super.lib.makeLibraryPath [ super.gnome.gtk ]}"
  '';

  buildInputs = [ super.makeWrapper ];
}
