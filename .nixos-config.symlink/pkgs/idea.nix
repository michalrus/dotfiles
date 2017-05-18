super: self:

super.idea // {
  idea-community = super.idea.idea-community.overrideDerivation (oldAttrs: {
    src = super.fetchurl {
      url = "https://download.jetbrains.com/idea/ideaIC-2017.1.3-no-jdk.tar.gz";
      sha256 = "1k948xd5k6j4l8rik294pcbbgd6qympc9cxjlspda9g3ns147v8n";
    };
  });
}
