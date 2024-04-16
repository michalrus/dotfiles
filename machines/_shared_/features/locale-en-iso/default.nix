{ ... }:

{
  i18n = {
    defaultLocale = "en_US.UTF-8";
    extraLocaleSettings = {
      LC_TIME = "en_GB.UTF-8"; # for 24 h
      LC_PAPER = "en_GB.UTF-8"; # for ISO A4 instead of Letter
      LC_MEASUREMENT = "en_GB.UTF-8"; # for metric
    };
  };
}
