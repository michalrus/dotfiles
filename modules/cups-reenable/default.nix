{pkgs, ...}: let
  nme = "cups-reenable";
in {
  # For some reason some printers get paused after something
  # happens. I don’t care. See
  # <https://superuser.com/questions/280396/how-to-resume-cups-printer-from-command-line/280400#280400>
  # for details. Apparently, this is the solution. I tried
  # `services.printing.extraConf = "ErrorPolicy retry-this-job"`, but
  # this doesn’t work. So let’s do it stupidly.
  systemd.services."${nme}" = {
    after = ["cups.service"];
    wantedBy = ["multi-user.target"];
    path = with pkgs; [cups.out];
    script = ''
      while true ; do
        if lpstat -p | grep -qF 'disabled since' ; then
          echo 'Found a disabled printer; re-enabling all.'
          ls /etc/cups/ppd | cut -d. -f1 | sort -u | xargs -I{} cupsenable {}
        fi
        sleep 10
      done
    '';
  };
}
