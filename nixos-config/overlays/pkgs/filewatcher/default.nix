{ lib, bundlerApp }:

bundlerApp {
  pname = "filewatcher";
  gemdir = ./.;
  exes = [ "filewatcher" ];

  meta = with lib; {
    description = "Command line utility to perform actions when files are updated, added or deleted.";
    homepage    = https://github.com/thomasfl/filewatcher;
    license     = licenses.mit;
    maintainers = [ maintainers.michalrus ];
    platforms   = platforms.all;
  };
}
