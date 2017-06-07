super: self:

super.idea // {
  idea-community = super.idea.idea-community.overrideDerivation (oldAttrs: {
    src = super.fetchurl {
      url = "https://download.jetbrains.com/idea/ideaIC-2017.1.4-no-jdk.tar.gz";
      sha256 = "1c3whrlb5wl0yi4gyfzsj6qy0njll0qgcy320dcalkiwz7izr47a";
    };
  });
}
