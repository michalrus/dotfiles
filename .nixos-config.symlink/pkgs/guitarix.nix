super: self:

super.guitarix.overrideDerivation (oldAttrs: rec {
  name = "guitarix-${version}";
  version = "0.35.1";

  src = super.fetchurl {
    url = "mirror://sourceforge/guitarix/guitarix2-${version}.tar.xz";
    sha256 = "066qva1zk63qw60s0vbi9g9jh22ljw67p91pk82kv11gw24h3vg6";
  };

  buildInputs = with super; [ makeWrapper glib gsettings_desktop_schemas ];

  preFixup = ''
    wrapProgram $out/bin/guitarix \
      --prefix GIO_EXTRA_MODULES : "${super.glib_networking.out}/lib/gio/modules" \
      --prefix XDG_DATA_DIRS : "${super.gsettings_desktop_schemas}/share/gsettings-schemas/${super.gsettings_desktop_schemas.name}"
  '';
})
