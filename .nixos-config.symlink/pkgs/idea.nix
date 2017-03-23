super: self:

super.idea // {
  idea-community = super.idea.idea-community.overrideDerivation (oldAttrs: {
    src = super.fetchurl {
      url = "https://download.jetbrains.com/idea/ideaIC-2017.1-no-jdk.tar.gz";
      sha256 = "04v5yxg44mvzf348dscjwk0q9jn0gwdjmgivqc3xylpdqsrigyxk";
    };
  });
}
