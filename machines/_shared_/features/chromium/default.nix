{ flake, config, lib, pkgs, ... }:

let
  unfree-unstable = import flake.inputs.nixpkgs-unstable.outPath {
    inherit (pkgs.stdenv.hostPlatform) system; config.allowUnfree = true;
  };
in

{
  # FIXME: No custom ~/.XCompose definitions in Chromium on native Wayland
  # FIXME: <https://issues.chromium.org/issues/40272818>
  # FIXME: <https://fcitx-im.org/wiki/Using_Fcitx_5_on_Wayland#Chromium_.2F_Electron>

  environment.etc = let
    policies = {
      ExtensionManifestV2Availability = 2;
    };
  in {
    "chromium/policies/managed/default.json".text = __toJSON policies;
    "opt/chrome/policies/managed/default.json".text = __toJSON policies;
  };

  home-manager.sharedModules = [
    {
      programs.chromium = {
        enable = true;
        package = unfree-unstable.chromium.override {
          enableWideVine = true;
        };
        commandLineArgs = [
          "--ozone-platform-hint=auto" # native Wayland
          "--enable-native-notifications"
        ];
        dictionaries = [
          flake.packages.${pkgs.stdenv.hostPlatform.system}.hunspell-dictionaries-chromium-pl
          pkgs.hunspellDictsChromium.en_US
        ];
        extensions = [
          { id = "cjpalhdlnbpafiamejdnhcphjbkeiagm"; } # ublock-origin
          { id = "dneaehbmnbhcippjikoajpoabadpodje"; } # old-reddit-redirect
          { id = "naepdomgkenhinolocfifgehidddafch"; } # browserpass # FIXME: move to ../_shared_/home/password-store
          { id = "ejddcgojdblidajhngkogefpkknnebdh"; } # AutoplayStopper
          { id = "mnjggcdmjocbbbhaepdhchncahnbgone"; } # SponsorBlock
        ];
      };

      home.packages = [
        (pkgs.writeShellApplication {
          name = "chromium-novpn";
          text = ''
            exec chromium-browser --proxy-server="socks5://10.77.2.1:1080" --user-data-dir="$HOME/.config/chromium/Profile-NoVPN" "$@"
          '';
        })
      ];
    }
  ];
}
