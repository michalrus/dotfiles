super: self:

super.idea // {
  idea-community = super.idea.idea-community.overrideDerivation (oldAttrs: {
    src = super.fetchurl {
      url = "https://download.jetbrains.com/idea/ideaIC-2017.1.2-no-jdk.tar.gz";
      sha256 = "08r33y32qrfylcvnbxljyjd061gkghm20br7m41xjgmn6y8ry03p";
    };
  });
}
