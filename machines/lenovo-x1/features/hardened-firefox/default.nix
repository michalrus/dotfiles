{pkgs, ...}: let
  pkgs-oldstable = import (
    pkgs.fetchFromGitHub {
      owner = "NixOS";
      repo = "nixpkgs";
      rev = "12d9950bf47e0ac20d4d04e189448ee075242117";
      sha256 = "09wy33zbzxj33296ddrrb79630kxpj1c3kiv38zs4wrw24206c2v";
    }
  ) {inherit (pkgs.stdenv.hostPlatform) system;};

  makeWrapped = args @ {
    localAutocompletePort,
    extraPrefs,
  }:
    pkgs-oldstable.wrapFirefox (unwrapped.override args) {};

  unwrapped = pkgs-oldstable.callPackage (
    {
      runCommand,
      writeText,
      writeScript,
      firefox-unwrapped,
      fetchurl,
      localAutocompletePort ? 999999999,
      extraPrefs ? "",
    }: let
      mkLocalAutocomplete = name: "http://127.0.0.1:${toString localAutocompletePort}/${name}?q={searchTerms}";

      emptySearchEngineName = "(empty default search engine)";

      # TODO: `zeroWidthSpace` is needed, because apparently we can’t redefine the default engine, cf. <https://github.com/mozilla/gecko-dev/blob/aaac9a77dd456360551dd764ffba4ca4899dcb56/browser/components/enterprisepolicies/Policies.jsm#L665-L739>
      searchEngines = let
        zeroWidthSpace = "​";
        googleIcon = {
          mimetype = "image/x-icon";
          url = "https://www.google.com/favicon.ico";
          sha256 = "0w070j6sngsk3045kr737cap40ny3p5ayzqr7qhk95hmh04659bd";
        };
      in [
        {
          Name = emptySearchEngineName;
          URLTemplate = "https://unused/search?{searchTerms}";
          SuggestURLTemplate = "https://unused/complete?{searchTerms}";
        }
        {
          Name = "Google" + zeroWidthSpace;
          Alias = "g";
          URLTemplate = "https://www.google.com/search?hl=en&q={searchTerms}";
          SuggestURLTemplate = "https://www.google.com/complete/search?client=firefox&q={searchTerms}&hl=en";
          IconURL = googleIcon;
        }
        {
          Name = "DuckDuckGo" + zeroWidthSpace;
          Alias = "d";
          URLTemplate = "https://duckduckgo.com/?q={searchTerms}&kl=wt-wt&kp=-2";
          SuggestURLTemplate = "https://ac.duckduckgo.com/ac/?q={searchTerms}&type=list&kl=wt-wt&kp=-2";
          IconURL = {
            mimetype = "image/x-icon";
            url = "https://duckduckgo.com/favicon.ico";
            sha256 = "08x9bcgvdzpf5b2l360llg3cylxalp47wakavbkb3w8dpm29miiy";
          };
        }
        {
          Name = "Wikipedia (en)" + zeroWidthSpace;
          Alias = "wiki";
          URLTemplate = "https://en.wikipedia.org/wiki/Special:Search?search={searchTerms}";
          SuggestURLTemplate = "https://en.wikipedia.org/w/api.php?action=opensearch&search={searchTerms}";
          IconURL = {
            mimetype = "image/x-icon";
            url = "https://en.wikipedia.org/favicon.ico";
            sha256 = "0276smg3zvdzy7gmnavr9yni28chrxcpsz8qylnfcmv849hxwf1n";
          };
        }
        {
          Name = "YouTube";
          Alias = "yt";
          URLTemplate = "https://www.youtube.com/results?search_query={searchTerms}";
          SuggestURLTemplate = "http://suggestqueries.google.com/complete/search?client=firefox&ds=yt&q={searchTerms}";
          IconURL = {
            mimetype = "image/x-icon";
            url = "https://youtube.com/favicon.ico";
            sha256 = "07cip1niccc05p124xggbmrl9p3n9kvzcinmkpakcx518gxd1ccb";
          };
        }
        {
          Name = "Google Images";
          Alias = "im";
          URLTemplate = "https://www.google.com/search?tbm=isch&hl=en&q={searchTerms}";
          SuggestURLTemplate = "https://images.google.com/complete/search?client=firefox&q={searchTerms}&ds=i&hl=en";
          IconURL = googleIcon;
        }
        {
          Name = "Google Maps";
          Alias = "maps";
          URLTemplate = "https://maps.google.com/maps?hl=en&q={searchTerms}";
          IconURL = googleIcon;
        }
        {
          Name = "Google Scholar";
          Alias = "scholar";
          URLTemplate = "https://scholar.google.com/scholar?hl=en&q={searchTerms}";
          SuggestURLTemplate = mkLocalAutocomplete "scholar";
          IconURL = googleIcon;
        }
        {
          Name = "GitHub";
          Alias = "gh";
          URLTemplate = "https://github.com/search?q={searchTerms}";
          IconURL = {
            mimetype = "image/x-icon";
            url = "https://github.com/favicon.ico";
            sha256 = "1y4mqsdrxzfzvdq2q1rw07v41lbwnmrpnvkqy480444ns4vk5r1f";
          };
        }
        {
          Name = "Hoogle";
          Alias = "hoogle";
          URLTemplate = "https://www.haskell.org/hoogle/?hoogle={searchTerms}";
          IconURL = {
            mimetype = "image/png";
            url = "https://www.haskell.org/hoogle/res/favicon.png";
            sha256 = "11yshd103fqg1ppwic528cnkjab8wd35hsrinr9njrnphqw75lw8";
          };
        }
        {
          Name = "Oxford Dictionaries";
          Alias = "oxford";
          URLTemplate = "https://en.oxforddictionaries.com/search?utf8=%E2%9C%93&filter=dictionary&query={searchTerms}";
          IconURL = {
            mimetype = "image/png";
            url = "https://en.oxforddictionaries.com/favicon-32x32.png";
            sha256 = "1src7jmyzia18gkqy5dd38bi2859d4qxj4vdwswfm5hiz0wnkmy4";
          };
        }
        {
          Name = "Merriam-Webster Dictionary";
          Alias = "mw";
          URLTemplate = "https://www.merriam-webster.com/dictionary/{searchTerms}";
          IconURL = {
            mimetype = "image/png";
            url = "https://www.merriam-webster.com/favicon.png";
            sha256 = "19wv9fjym36104wx9q0hilx7xq5i2d0jsz5xzb7rcyx8z7bbpgnh";
          };
        }
        {
          Name = "The Free Dictionary";
          Alias = "tfd";
          URLTemplate = "https://www.thefreedictionary.com/{searchTerms}";
          SuggestURLTemplate = "https://www.thefreedictionary.com/_/search/suggest.ashx?query={searchTerms}";
          IconURL = {
            mimetype = "image/x-icon";
            url = "https://img.tfd.com/favicon.ico";
            sha256 = "0zkqb24i1i80khw6vbpmdrdpn3ayraiw5h3ha2d6hvvz0d1mvydm";
          };
        }
        {
          Name = "Słownik angielsko-polski Diki";
          Alias = "diki";
          URLTemplate = "https://www.diki.pl/slownik-angielskiego?q={searchTerms}";
          SuggestURLTemplate = mkLocalAutocomplete "diki";
          IconURL = {
            mimetype = "image/x-icon";
            url = "https://www.diki.pl/favicon.ico";
            sha256 = "1sc90g1fvfi6fa9rjn7psrra4dlb5qyxy4xnyh9fmjpnq47fyxv7";
          };
        }
        {
          Name = "Blockchain Explorer";
          Alias = "block";
          URLTemplate = "https://www.blockchain.com/search?search={searchTerms}";
          IconURL = {
            mimetype = "image/x-icon";
            url = "https://www.blockchain.com/favicon.ico";
            sha256 = "0n4iwsvyh9n4yh6hjhqjhs2d3hi4arjm24gg1l0h6lbmxkplm7kf";
          };
        }
        {
          Name = "Allegro.pl";
          Alias = "allegro";
          URLTemplate = "https://allegro.pl/listing?string={searchTerms}";
          SuggestURLTemplate = mkLocalAutocomplete "allegro";
          IconURL = {
            mimetype = "image/x-icon";
            url = "https://allegro.pl/favicon.ico";
            sha256 = "1y1xv9sl8sangs6hfzyhaj25alqlslj3yp2p88hyd0y4f3rnwx0l";
          };
        }
        {
          Name = "Leafly";
          Alias = "leafly";
          URLTemplate = "https://www.leafly.com/search?q={searchTerms}&cat=strain";
          SuggestURLTemplate = mkLocalAutocomplete "leafly";
          IconURL = {
            mimetype = "image/x-icon";
            url = "https://www.leafly.com/favicon.ico";
            sha256 = "1vn1kjc8s6rjw6825fsrqv72d6ffk4rsmvlzhih7jqclim04l9cy";
          };
        }
        {
          Name = "IleWaży.pl";
          Alias = "iw";
          URLTemplate = "http://www.ilewazy.pl/produkty/page/1/q/{searchTerms}";
          IconURL = {
            mimetype = "image/x-icon";
            url = "http://www.ilewazy.pl/favicon.ico";
            sha256 = "162xvfs49jc749sw6z6jd4z3hdppj1nasqz6y8aykiaf01vl61n0";
          };
        }
        {
          Name = "Słownik języka polskiego PWN";
          Alias = "sjp";
          URLTemplate = "https://sjp.pwn.pl/szukaj/{searchTerms}.html";
          SuggestURLTemplate = mkLocalAutocomplete "sjp";
          IconURL = {
            mimetype = "image/x-icon";
            url = "https://sjp.pwn.pl/favicon.ico";
            sha256 = "1j7f0zifpsbgih9m340lz9j2bmk3slbw1y2gnrnqkx85qjziispn";
          };
        }
        {
          Name = "Filmweb";
          Alias = "filmweb";
          URLTemplate = "https://www.filmweb.pl/search?q={searchTerms}";
          SuggestURLTemplate = mkLocalAutocomplete "filmweb";
          IconURL = {
            mimetype = "image/x-icon";
            url = "https://www.filmweb.pl/favicon.ico";
            sha256 = "029rq2sp2nrv1zy7syxvmzy035ydw4qk1v5azpkz78q72x2vyc1v";
          };
        }
        {
          Name = "IMDb";
          Alias = "imdb";
          URLTemplate = "https://www.imdb.com/find?q={searchTerms}";
          SuggestURLTemplate = mkLocalAutocomplete "imdb";
          IconURL = {
            mimetype = "image/x-icon";
            url = "https://www.imdb.com/favicon.ico";
            sha256 = "0vvfll9srd21cdrlrkspqfyzv4sadsdral4j3rvdiqqzcw6g053p";
          };
        }
      ];

      searchEnginesIconsNoGC = writeText "searchEnginesIconsNoGC" (toString (map ({IconURL ? null, ...}: (
          if isNull IconURL
          then ""
          else (toDataUrl IconURL).noGC
        ))
        searchEngines));

      policies = writeText "policies.json" (builtins.toJSON {
        policies = {
          DNSOverHTTPS = {
            Enabled = false;
            Locked = true;
          };
          Bookmarks = [];
          DisableMasterPasswordCreation = true; # use an external passwd manager
          DisableFeedbackCommands = true;
          DisableFirefoxAccounts = true;
          DisableFirefoxStudies = true;
          DisableForgetButton = true;
          DisableFormHistory = true;
          DisablePocket = true;
          DisableProfileImport = true;
          DisableProfileRefresh = true;
          DisableTelemetry = true;
          DontCheckDefaultBrowser = true;
          EnableTrackingProtection = {
            Value = true;
            Locked = true;
          };
          NoDefaultBookmarks = true;
          OfferToSaveLogins = false;
          Homepage = {
            URL = "about:blank";
            StartPage = "previous-session";
            Locked = true;
          };
          PopupBlocking = {
            Default = true;
            Locked = true;
          };
          FlashPlugin = {
            Default = false;
            Locked = true;
          };
          OverrideFirstRunPage = "";
          OverridePostUpdatePage = "";
          #Proxy = { Mode = "none"; Locked = true; };
          RequestedLocales = ["en-US.UTF-8"];

          SearchEngines = {
            # TODO: how to remove all of them, without listing explicitly?
            # TODO: post a bug to have another policy `SearchEngines.RemoveAllAndAddDefault`
            # Note: if search.json.mozlz4 is removed before Firefox runs, we *can* delete Google. Otherwise, not.
            Remove = [
              "Amazon.com"
              "Bing"
              "DuckDuckGo"
              "eBay"
              "Google"
              "Twitter"
              "Wikipedia (en)"
            ];

            # Have an empty engine as a default, because the browser sends search suggestions to the default engine, even when input without a keyword (even when the pref. `keyword.enabled` is false).
            Default = emptySearchEngineName;
            PreventInstalls = true;

            Add = map (eng @ {IconURL ? null, ...}:
              eng
              // (
                if isNull IconURL
                then {}
                else {IconURL = (toDataUrl IconURL).url;}
              ))
            searchEngines;
          };

          Permissions = {
            Location = {
              BlockNewRequests = true;
              Locked = true;
            };
            Notifications = {
              BlockNewRequests = true;
              Locked = true;
            };
          };

          # https://addons.mozilla.org/en-US/firefox/addon/container-outgoing-links/

          Extensions = {
            Install = [
              "https://addons.mozilla.org/firefox/downloads/latest/multi-account-containers/"
              #              "https://addons.mozilla.org/firefox/downloads/latest/temporary-containers/"
              "https://addons.mozilla.org/firefox/downloads/latest/ublock-origin/"
              "https://addons.mozilla.org/firefox/downloads/latest/https-everywhere/"
              "https://addons.mozilla.org/firefox/downloads/latest/add-url-to-window-title/"
              "https://addons.mozilla.org/firefox/downloads/latest/decentraleyes/"
              "https://addons.mozilla.org/firefox/downloads/latest/utm-tracking-token-stripper/"
              "https://addons.mozilla.org/firefox/downloads/latest/google-search-link-fix/"
              "https://addons.mozilla.org/firefox/downloads/latest/i-dont-care-about-cookies/"
            ];
            Locked = [
              "@testpilot-containers"
              #              "{c607c8df-14a7-4f28-894f-29e8722976af}" # temporary-containers
              "uBlock0@raymondhill.net"
              "https-everywhere@eff.org"
              "autt@ericgoldman.name" # add-url-to-window-title
              "jid1-BoFifL9Vbdl2zQ@jetpack" # decentraleyes
              "{9fda17be-849d-4f5b-a326-28d25f0f6d29}" # utm-tracking-token-stripper
              "jid0-XWJxt5VvCXkKzQK99PhZqAn7Xbg@jetpack" # google-search-link-fix
              "jid1-KKzOGWgsW3Ao4Q@jetpack" # i-dont-care-about-cookies
            ];
          };
        };
      });

      # <https://support.mozilla.org/en-US/kb/customizing-firefox-using-autoconfig>
      autoconfig = writeText "autoconfig.js" ''
        pref("general.config.obscure_value", 0);
        pref("general.config.filename", "firefox.cfg");
      '';

      cfgEnableDRM = ''
        // Allow unfree DRM on some accounts:
        lockPref("media.eme.enabled", true);
        lockPref("browser.eme.ui.enabled", true);
        lockPref("media.gmp-widevinecdm.enabled", true);
        lockPref("media.gmp-widevinecdm.visible", true);
      '';

      cfgUX = ''
        // Always recreate search engines from policy.
        clearPref("browser.policies.runOncePerModification.addSearchEngines");
        clearPref("browser.policies.runOncePerModification.removeSearchEngines");
        clearPref("browser.policies.runOncePerModification.setDefaultSearchEngine");

        // Block unfree DRM.
        lockPref("media.eme.enabled", false);
        lockPref("browser.eme.ui.enabled", false);
        lockPref("media.gmp-widevinecdm.enabled", false);
        lockPref("media.gmp-widevinecdm.visible", false);

        // Mozilla fsck up with extension intermediary signature key expiring.
        lockPref("xpinstall.signatures.required", false);

        // SearchEngines
        lockPref("keyword.enabled", false); // ! Require a keyword before sending URLBar to a search engine.
        lockPref("browser.urlbar.oneOffSearches", false);
        lockPref("browser.search.update", false);
        lockPref("browser.search.searchEnginesURL", "");

        // UX.
        lockPref("dom.ipc.processCount", 8); // Use more processes for rendering (more RAM, but faster).
        pref("browser.ctrlTab.recentlyUsedOrder", false);
        lockPref("browser.startup.page", 3);
        pref("browser.urlbar.clickSelectsAll", true);
        pref("browser.urlbar.doubleClickSelectsAll", false);
        lockPref("clipboard.plainTextOnly", true);
        lockPref("devtools.selfxss.count", 999999999); // Allow pasting into dev console.
        lockPref("dom.disable_open_during_load", true); // Disable pop-ups.
        lockPref("general.smoothScroll", false); // Using MOZ_USE_XINPUT2 env-var.
        lockPref("general.warnOnAboutConfig", false);
        lockPref("media.autoplay.allow-muted", false);
        lockPref("media.autoplay.default", 1);
        lockPref("pref.privacy.disable_button.view_passwords", false);
        lockPref("signon.rememberSignons", false);
        lockPref("browser.formfill.enable", false);
        lockPref("browser.formfill.expire_days", 0);
        lockPref("signon.autofillForms", false);
        lockPref("signon.autofillForms.http", false);
        lockPref("signon.formlessCapture.enabled", false);
        lockPref("ui.context_menus.after_mouseup", true);
        lockPref("browser.shell.checkDefaultBrowser", false);
        pref("findbar.highlightAll", true);
        pref("browser.sessionstore.max_tabs_undo", 25);
        pref("browser.sessionstore.max_windows_undo", 10);
        lockPref("browser.sessionstore.max_serialize_back", -1);
        lockPref("browser.sessionstore.max_serialize_forward", -1);
        lockPref("browser.sessionstore.max_resumed_crashes", -1); // Don’t limit how many pages have to crash to dislay about:sessionrestore instead of restoring. Clean SIGTERM is considered to “crash” the pages, so… remove the limit, annoying.
        lockPref("mousewheel.with_control.action", 1); // Don’t use any scroll modifiers, annoying.
        lockPref("mousewheel.with_alt.action", 1);
        lockPref("mousewheel.with_meta.action", 1);
        lockPref("mousewheel.with_shift.action", 1);
        lockPref("mousewheel.with_win.action", 1);
        pref("general.autoScroll", true);

        // Control
        lockPref("browser.urlbar.trimURLs", false);
        //lockPref("dom.event.clipboardevents.enabled", false); // breaks Facebook® Messenger™
        //lockPref("dom.event.contextmenu.enabled", false);
        lockPref("network.IDN_show_punycode", true);

        // Clean startup.
        lockPref("browser.startup.firstrunSkipsHomepage", false);
        lockPref("startup.homepage_welcome_url", "about:blank");

        // Extensions
        lockPref("extensions.legacy.enabled", false);
        lockPref("extensions.legacy.exceptions", "");
        lockPref("extensions.screenshots.upload-disabled", true);
        lockPref("extensions.systemAddonSet", "{\"schema\":1,\"addons\":{}}");
      '';

      cfgTelemetryOff = ''
        // Telemetry, reports.
        lockPref("breakpad.reportURL", ""); // Disable sending Firefox crash reports to Mozilla servers
        lockPref("browser.crashReports.unsubmittedCheck.enabled", false);
        lockPref("browser.discovery.enabled", false);
        lockPref("browser.tabs.crashReporting.sendReport", false);
        lockPref("datareporting.healthreport.service.enabled",		false);
        lockPref("datareporting.healthreport.uploadEnabled",		false);
        lockPref("datareporting.policy.dataSubmissionEnabled",		false);
        lockPref("experiments.enabled", false);
        lockPref("experiments.manifest.uri", "");
        lockPref("experiments.supported", false);
        lockPref("toolkit.telemetry.archive.enabled", false);
        lockPref("toolkit.telemetry.enabled", false);
        lockPref("toolkit.telemetry.unified", false);
        lockPref("browser.selfsupport.url", "");
        lockPref("extensions.shield-recipe-client.enabled", false);
        lockPref("app.shield.optoutstudies.enabled", false);
      '';

      cfgPrivacy = ''
        //
        // User.js, based on <https://github.com/pyllyukko/user.js/>, but more for… real people.
        //

        // Security.
        lockPref("browser.urlbar.filter.javascript", true);
        lockPref("plugin.state.flash", 0);
        lockPref("plugin.state.java", 0);
        lockPref("plugins.click_to_play", true);
        lockPref("security.fileuri.strict_origin_policy", true);
        lockPref("security.mixed_content.block_active_content", true); // JavaScript vulnerable to MitM — block it!
        pref("security.mixed_content.block_display_content", true); // But that’s only images and CSS?
        lockPref("security.xpconnect.plugin.unrestricted", false); // ?
        lockPref("devtools.webide.enabled", false);
        lockPref("devtools.webide.autoinstallADBHelper", false);
        lockPref("devtools.webide.autoinstallFxdtAdapters", false);
        lockPref("devtools.debugger.remote-enabled", false);
        pref("devtools.chrome.enabled", false);
        lockPref("devtools.debugger.force-local", true);
        lockPref("network.allow-experiments", false); // Disallow Necko to do A/B testing
        lockPref("browser.uitour.enabled", false);
        lockPref("browser.search.update", false); // Don’t update search engines.
        lockPref("security.insecure_password.ui.enabled", true);
        pref("media.peerconnection.enabled", true); // false = disable WebRTC
        lockPref("media.peerconnection.ice.default_address_only",	true); // Firefox 42-51
        lockPref("media.peerconnection.ice.no_host", true); // Firefox >= 52

        // Crypto.
        lockPref("network.stricttransportsecurity.preloadlist", true);
        lockPref("security.cert_pinning.enforcement_level", 2); // "2. Strict. Pinning is always enforced."
        lockPref("security.ssl.treat_unsafe_negotiation_as_broken", true);
        lockPref("security.ssl.errorReporting.automatic", false);

        // Decrease entropy.
        //lockPref("privacy.resistFingerprinting", true);
        lockPref("intl.accept_languages", "en-us,en");
        lockPref("browser.search.countryCode", "US");
        lockPref("browser.search.geoSpecificDefaults", false);
        lockPref("browser.search.geoip.url", "");
        lockPref("browser.search.region", "US");
        lockPref("privacy.donottrackheader.enabled", false); // Yes, false! Useless + don’t stand out.
        lockPref("dom.network.enabled", false);
        lockPref("dom.netinfo.enabled", false);
        //lockPref("dom.enable_performance", false);

        // But increase entropy for performance and UX…
        lockPref("privacy.resistFingerprinting", false);
        lockPref("dom.enable_performance", true);

        // Performance.
        lockPref("layers.acceleration.force-enabled", true);

        // Privacy.
        lockPref("privacy.firstparty.isolate", true);
        lockPref("privacy.trackingprotection.enabled", true);
        lockPref("browser.contentblocking.category", "strict");
        lockPref("network.cookie.cookieBehavior", 4); // Reject known trackers, accept 1st and 3rd party.
        lockPref("network.cookie.thirdparty.sessionOnly", true); // Remove 3rd party on exit.
        lockPref("network.cookie.lifetimePolicy", 0); // The cookie's lifetime is supplied by the server.
        lockPref("browser.cache.offline.enable", false);
        lockPref("dom.vibrator.enabled", false);
        lockPref("dom.vr.enabled", false);
        lockPref("dom.gamepad.enabled", false);
        lockPref("dom.enable_resource_timing", false);
        lockPref("browser.send_pings", false);
        lockPref("device.sensors.enabled", false);
        lockPref("media.webspeech.synth.enabled", false);
        lockPref("media.webspeech.recognition.enable", false);
        lockPref("beacon.enabled", false);
        lockPref("dom.telephony.enabled", false);
        lockPref("dom.battery.enabled", false);
        lockPref("camera.control.face_detection.enabled", false);
        lockPref("accessibility.force_disabled", 1);
        lockPref("browser.fixup.alternate.enabled", false);
        lockPref("browser.fixup.hide_user_pass", true);
        lockPref("browser.newtab.preload", false);
        lockPref("browser.newtabpage.activity-stream.enabled", false);
        lockPref("browser.newtabpage.activity-stream.asrouter.userprefs.cfr", false);
        lockPref("browser.newtabpage.activity-stream.disableSnippets", true);
        lockPref("browser.newtabpage.activity-stream.feeds.section.highlights", false);
        lockPref("browser.newtabpage.activity-stream.feeds.section.topstories", false);
        lockPref("browser.newtabpage.activity-stream.feeds.section.topstories.options", "{}");
        lockPref("browser.newtabpage.activity-stream.feeds.sections", false);
        lockPref("browser.newtabpage.activity-stream.feeds.snippets", false);
        lockPref("browser.newtabpage.activity-stream.feeds.topsites", false);
        lockPref("browser.newtabpage.activity-stream.filterAdult", false);
        lockPref("browser.newtabpage.activity-stream.prerender", false);
        lockPref("browser.newtabpage.activity-stream.showSponsored", false);
        lockPref("browser.newtabpage.enabled", false);
        lockPref("browser.safebrowsing.downloads.enabled", false);
        lockPref("browser.safebrowsing.downloads.remote.enabled", false); // Disable querying Google Application Reputation database for downloaded binary files
        lockPref("browser.startup.homepage", "about:blank");
        lockPref("datareporting.healthreport.uploadEnabled", false);
        lockPref("extensions.pocket.api", "");
        lockPref("extensions.pocket.enabled", false);
        lockPref("extensions.pocket.oAuthConsumerKey", "");
        lockPref("extensions.pocket.site", "");
        lockPref("geo.enabled", false);
        lockPref("geo.wifi.uri", "");
        lockPref("geo.wifi.logging.enabled", false);
        lockPref("network.dns.disablePrefetch", true);
        lockPref("network.http.sendRefererHeader", 2); // Send on all requests...
        lockPref("network.http.referer.XOriginPolicy", 2); // ... but only for same-origin, since not sending it at all breaks some CSRF defenses (e.g. Last.fm).
        lockPref("network.manage-offline-status", false);
        lockPref("network.predictor.enabled", false);
        lockPref("network.prefetch-next", false);
        lockPref("network.proxy.socks_remote_dns", true);
        lockPref("network.trr.mode", 5); // <https://wiki.mozilla.org/Trusted_Recursive_Resolver>
        lockPref("network.trr.uri", "");
        lockPref("extensions.blocklist.url", "https://blocklist.addons.mozilla.org/blocklist/3/%APP_ID%/%APP_VERSION%/"); // Decrease system information leakage to Mozilla blocklist update servers

        // Auto-connections.
        lockPref("network.prefetch-next", false);
        lockPref("network.dns.disablePrefetch", true);
        lockPref("network.dns.disablePrefetchFromHTTPS", true);
        lockPref("network.predictor.enabled", false);
        lockPref("network.dns.blockDotOnion", false); // true → don’t send *.onion queries to DNS
        lockPref("browser.casting.enabled", false);
        lockPref("network.http.speculative-parallel-limit", 0);
        lockPref("browser.aboutHomeSnippets.updateUrl", "");
        lockPref("network.captive-portal-service.enabled", false);
      '';

      firefox-cfg = writeText "firefox.cfg" ''
        // IMPORTANT: Start your code on the 2nd line
        ${cfgUX}
        ${cfgTelemetryOff}
        ${cfgPrivacy}
        ${extraPrefs}
      '';

      toDataUrl = args @ {
        url,
        sha256,
        mimetype,
      }: let
        dled = fetchurl {inherit (args) url sha256;};
        encoded = runCommand "toDataUrl" {} ''base64 -w 0 ${dled} >$out'';
      in {
        url = "data:" + mimetype + ";base64," + builtins.readFile encoded;
        noGC = [dled encoded];
      };

      preStartScript = writeScript "firefox-pre-start" ''
        mkdir -p $HOME/.mozilla/firefox

        # Always recreate search engines from policy.
        find $HOME/.mozilla/firefox/ -name search.json.mozlz4 -exec rm {} \;

        systemctl --user start firefox-autocomplete.service &
      '';
    in
      runCommand firefox-unwrapped.name {
        inherit (firefox-unwrapped) passthru meta;
        inherit cfgPrivacy cfgTelemetryOff cfgUX cfgEnableDRM policies;
      } ''
        cp -a ${firefox-unwrapped} $out
        chmod -R u+rwX $out

        substituteInPlace $out/bin/firefox \
          --replace ${firefox-unwrapped} $out \
          --replace 'exec ' '${preStartScript} ; exec '
        ln -sf $out/lib/firefox/firefox $out/bin/.firefox-wrapped

        ln -s ${searchEnginesIconsNoGC} $out/searchEnginesIconsNoGC.keep

        mkdir -p $out/lib/firefox/distribution
        ln -s ${policies} $out/lib/firefox/distribution/policies.json

        mkdir -p $out/lib/firefox/defaults/pref
        chmod 755 $out/lib/firefox/defaults/pref
        echo ----------------------------------------------------------
        ls -alh $out/lib/firefox/defaults/pref

        echo ----------------------------------------------------------
        ln -s ${autoconfig} $out/lib/firefox/defaults/pref/autoconfig.js'~'

        ln -s ${firefox-cfg} $out/lib/firefox/firefox.cfg
      ''
  ) {};
in {
  nixpkgs.overlays = [
    (_: _: {
      hardened-firefox = {
        inherit makeWrapped unwrapped;
      };
    })
  ];
}
