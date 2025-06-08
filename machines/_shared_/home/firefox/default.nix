{ config, flake, pkgs, lib, ... }:

let
  isMichal = config.home.username == "mw" || config.home.username == "m";
in

{
  home.packages = [
    (pkgs.writeShellApplication {
      name = "firefox-novpn";
      text = ''
        exec firefox -no-remote -P "$HOME/.mozilla/firefox/novpn" "$@"
      '';
    })
  ];

  home.sessionVariables = {
    MOZ_ENABLE_WAYLAND = "1";
    MOZ_USE_XINPUT2 = "1";
  };

  programs.firefox = {
    enable = true;
    languagePacks = ["en-US" "pl"];
    profiles = {
      default = {
        id = 0;
        isDefault = true;
        settings = {
          "findbar.highlightAll" = true;
        };
      };
      novpn = {
        id = 800;
        settings = {
          "network.proxy.type" = 1;
          "network.proxy.socks" = "10.77.2.1";
          "network.proxy.socks_port" = 1080;
          "network.proxy.socks_version" = 5;
          "network.proxy.socks_remote_dns" = true;
          "findbar.highlightAll" = true;
        };
      };
    };
    policies = {
      Extensions = {
        Install = [
          "https://addons.mozilla.org/firefox/downloads/latest/browserpass-ce/"
          "https://addons.mozilla.org/firefox/downloads/latest/ublock-origin/"
          "https://addons.mozilla.org/firefox/downloads/latest/sponsorblock/"
          "https://addons.mozilla.org/firefox/downloads/latest/old-reddit-redirect/"
          "https://addons.mozilla.org/firefox/downloads/latest/ctrl-number-to-switch-tabs/"
        ];
        Locked = [
          "browserpass@maximbaz.com" # browserpass-ce
          "uBlock0@raymondhill.net" # ublock-origin
          "sponsorBlocker@ajay.app" # sponsorblock
          "{9063c2e9-e07c-4c2c-9646-cfe7ca8d0498}" # old-reddit-redirect
          "{84601290-bec9-494a-b11c-1baa897a9683}" # ctrl-number-to-switch-tabs
        ];
      };
      ExtensionSettings = {
        "browserpass@maximbaz.com".private_browsing = true; # browserpass-ce
        "uBlock0@raymondhill.net".private_browsing = true; # ublock-origin
        "sponsorBlocker@ajay.app".private_browsing = true; # sponsorblock
        "{9063c2e9-e07c-4c2c-9646-cfe7ca8d0498}".private_browsing = true; # old-reddit-redirect
        "{84601290-bec9-494a-b11c-1baa897a9683}".private_browsing = true; # ctrl-number-to-switch-tabs
      };

      # Block 3rd-party cookies:
      Cookies = {
        Locked = true;
        Behavior = "reject-foreign";
        BehaviorPrivateBrowsing = "reject-foreign";
      };

      DNSOverHTTPS = { Enabled = false; Locked = true; };
      DisableFirefoxStudies = true;
      DisablePocket = true;
      DisableTelemetry = true;
      EnableTrackingProtection = { Value = true; Locked = true; };
      FlashPlugin = { Default = false; Locked = true; };
      Homepage = { URL = "about:blank"; StartPage = "previous-session"; Locked = true; };
      NoDefaultBookmarks = true;
      OverrideFirstRunPage = "";
      OverridePostUpdatePage = "";
      Permissions.Autoplay = { Default = "block-audio-video"; Locked = true; };
      Permissions.Location = { BlockNewRequests = true; Locked = true; };
      PopupBlocking = { Default = true; Locked = true; };
      RequestedLocales = ["en-US" "pl"];
      SkipTermsOfUse = true;

      UserMessaging = {
        "ExtensionRecommendations" = false;
        "FeatureRecommendations" = false;
        "UrlbarInterventions" = false;
        "SkipOnboarding" = false;
        "MoreFromMozilla" = false;
        "FirefoxLabs" = false;
        "Locked" = true;
      };

      Preferences = builtins.mapAttrs (_: value: { Value = value; Status = "locked"; }) ({
        "browser.newtabpage.enabled" = false;
        "datareporting.healthreport.uploadEnabled" = false;
        "browser.translations.automaticallyPopup" = false;

        # Keep extension settings as editable JSON files:
        "extensions.webextensions.ExtensionStorageIDB.enabled" = false;

        # Allow unfree DRM (Netflix etc.):
        "media.eme.enabled" = true;
        "browser.eme.ui.enabled" = true;
        "media.gmp-widevinecdm.enabled" = true;
        "media.gmp-widevinecdm.visible" = true;

        # Lost CPU cycles for a silly animation. Scrolling is still smooth on Wayland.
        "apz.overscroll.enabled" = false;
        "general.smoothScroll" = false;

        # Better performance:
        "gfx.webrender.all" = true;
        "media.ffmpeg.vaapi.enabled" = true;
      } // lib.optionalAttrs isMichal {
        # More aggressive background timer throttling (it doesn’t throttle WebSocket pages, unfortunately):
        "dom.timeout.throttling_delay" = 1000;
        "dom.min_background_timeout_value" = 30000;
        "dom.min_background_timeout_value_without_budget_throttling" = 30000;
        "dom.timeout.background_throttling_max_budget" = 20;
        "dom.timeout.budget_throttling_max_delay" = 30000;
        "dom.timeout.background_budget_regeneration_rate" = 500;
      });
    } // lib.optionalAttrs isMichal {
      AutofillAddressEnabled = false;
      AutofillCreditCardEnabled = false;
      Bookmarks = [];
      DisableFeedbackCommands = true;
      DisableFirefoxAccounts = true;
      DisableForgetButton = true;
      DisableFormHistory = true;
      DisableMasterPasswordCreation = true; # use an external passwd manager
      DisableProfileImport = true;
      DisableProfileRefresh = true;
      DisplayBookmarksToolbar = "never";
      DisplayMenuBar = "never";
      # DontCheckDefaultBrowser = true;
      HttpsOnlyMode = "force_enabled";
      OfferToSaveLogins = false;
      OfferToSaveLoginsDefault = false;
      PasswordManagerEnabled = false;
      PrimaryPassword = false;
    };
  };
}
