_: let
  policies = builtins.toJSON {
    HomepageIsNewTabPage = true;
    HomepageLocation = "about:blank";
    NewTabPageLocation = "chrome:newtab";

    DefaultCookiesSetting = 1; # Allow all sites to set local data
    #BlockThirdPartyCookies = true; # That would break too many sites.

    SSLVersionMin = "tls1";
    BuiltInDnsClientEnabled = false;

    DefaultPopupsSetting = 2; # Do not allow any site to show popups
    DefaultPluginsSetting = 2; # Block the Flash plugin
    RunAllFlashInAllowMode = false;
    AllowOutdatedPlugins = false;
    DefaultGeolocationSetting = 2; # Do not allow any site to track the users' physical location
    DefaultWebBluetoothGuardSetting = 2; # Do not allow any site to request access to Bluetooth devices via the Web Bluetooth API
    DefaultWebUsbGuardSetting = 2; # Do not allow any site to request access to USB devices via the WebUSB API

    BrowserAddPersonEnabled = false;
    BrowserGuestModeEnabled = false;
    BrowserSignin = 0; # Disable browser sign-in
    SecondaryGoogleAccountSigninAllowed = false;

    DefaultBrowserSettingEnabled = false;

    RestoreOnStartup = 1; # Restore the last session
    AllowDeletingBrowserHistory = false;
    AllowDinosaurEasterEgg = false;

    DownloadDirectory = "/home/$\{user_name}/Downloads";
    PromptForDownloadLocation = false;
    SafeBrowsingForTrustedSourcesEnabled = false;

    EditBookmarksEnabled = false;
    BookmarkBarEnabled = false;
    SavingBrowserHistoryDisabled = false;
    ShowAppsShortcutInBookmarkBar = false;
    ShowHomeButton = false;

    PasswordManagerEnabled = false;

    ImportAutofillFormData = false;
    ImportBookmarks = false;
    ImportHistory = false;
    ImportHomepage = false;
    ImportSavedPasswords = false;
    ImportSearchEngine = false;

    #IncognitoModeAvailability = 2; # Incognito mode forced # But you can’t edit settings then at all.

    MetricsReportingEnabled = false;
    UrlKeyedAnonymizedDataCollectionEnabled = false;
    NTPContentSuggestionsEnabled = false;
    NetworkPredictionOptions = false;
    PromotionalTabsEnabled = false;
    SafeSitesFilterBehavior = 0; # Do not filter sites for adult content
    SpellCheckServiceEnabled = false;
    SyncDisabled = true;
    TranslateEnabled = false;
    WebDriverOverridesIncompatiblePolicies = false;
    WebRtcEventLogCollectionAllowed = false;
    SafeBrowsingEnabled = false;
    BackgroundModeEnabled = false;

    SpellcheckEnabled = true;
    SpellcheckLanguage = ["en" "pl"];
    AllowedLanguages = ["en-US" "en" "pl"];
    ApplicationLocaleValue = "en-US";
    AutofillAddressEnabled = false;
    AutofillCreditCardEnabled = false;
    AutoplayAllowed = false;

    SearchSuggestEnabled = true;
    #DefaultSearchProviderSearchURL = "https://encrypted.google.com/search?q={searchTerms}&{google:RLZ}{google:originalQueryForSuggestion}{google:assistedQueryStats}{google:searchFieldtrialParameter}{google:searchClient}{google:sourceId}{google:instantExtendedEnabledParameter}ie={inputEncoding}";
    #DefaultSearchProviderSuggestURL = "https://encrypted.google.com/complete/search?output=chrome&q={searchTerms}";

    #ExtensionInstallBlacklist = ["*"];
    ExtensionInstallForcelist =
      # doesn’t apply to incognito?
      map (x: x + ";https://clients2.google.com/service/update2/crx")
      [
        "cjpalhdlnbpafiamejdnhcphjbkeiagm" # uBlock Origin
        "gcbommkclmclpchllfjekcdonpmejbdp" # HTTPS Everywhere
        "fdpohaocaechififmbbbbbknoalclacl" # Full Page Screen Capture
        "ignpacbgnbnkaiooknalneoeladjnfgb" # URL in title
      ];
    #ExtensionSettings = {};
  };
in {
  environment.etc."chromium/policies/managed/default.json".text = policies;
  environment.etc."opt/chrome/policies/managed/default.json".text = policies;
}
