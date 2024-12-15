{ config, flake, pkgs, lib, ... }:

{

  programs.gpg.enable = true;

  programs.gpg.settings = {
    no-greeting = true;
    keyid-format = "0xlong";
    keyserver = "hkp://keyserver.ubuntu.com";
  };

  home.sessionVariables = if pkgs.stdenv.isLinux then {
    SSH_AUTH_SOCK = "$HOME/.gnupg/S.gpg-agent.ssh";
  } else {};

  home.file.".gnupg/gpg-agent.conf".text = ''
    default-cache-ttl 0
    max-cache-ttl 0
    default-cache-ttl-ssh 0
    max-cache-ttl-ssh 0
    enable-ssh-support
    no-allow-external-cache
    ignore-cache-for-signing
    pinentry-program ${
      if pkgs.stdenv.isDarwin
      then pkgs.pinentry_mac + "/" + pkgs.pinentry_mac.binaryPath
      else lib.getExe pkgs.pinentry-gtk2
    }
    # XXX: grabbing doesn’t prevent other apps snooping on X11 input events, but it’s convenient wrt. focus
    # <https://theinvisiblethings.blogspot.com/2011/04/linux-security-circus-on-gui-isolation.html>
    grab
  '';

  targets = if pkgs.stdenv.isDarwin then {
    # FIXME: this still doesn’t work:
    # what ‘no-allow-external-cache’ should’ve done above:
    darwin.defaults."org.gpgtools.pinentry-mac".DisableKeychain = true;
  } else {};

}
