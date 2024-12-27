{ config, lib, pkgs, ... }:

{
  services.udisks2.enable = true;

  home-manager.sharedModules = [{
    home.packages = with pkgs; [
      pcmanfm
      evince
      qimgv
      udiskie
    ];

    xdg.mimeApps.enable = true;
    xdg.mimeApps.defaultApplications = let
      webBrowser = ["chromium-browser.desktop"];
      fileBrowser = ["pcmanfm.desktop"];
      pdfViewer = ["org.gnome.Evince.desktop"];
      imageViewer = ["qimgv.desktop"];
      videoPlayer = ["mpv.desktop"];
    in {
      "inode/directory" = fileBrowser;

      "x-scheme-handler/http" = webBrowser;
      "x-scheme-handler/https" = webBrowser;

      "application/pdf" = pdfViewer;

      "image/jpeg" = imageViewer;
      "image/png" = imageViewer;
      "image/gif" = imageViewer;
      "image/bmp" = imageViewer;
      "image/svg+xml" = imageViewer;
      "image/tiff" = imageViewer;

      "video/mp4" = videoPlayer;
      "video/x-matroska" = videoPlayer;
      "video/vnd.avi" = videoPlayer;
      "video/x-ms-wmv" = videoPlayer;
      "video/webm" = videoPlayer;
      "video/ogg" = videoPlayer;
      "video/3gpp" = videoPlayer;
      "video/3gpp2" = videoPlayer;
      "video/quicktime" = videoPlayer;
      "video/mpeg" = videoPlayer;
    };
  }];

  services.printing = {
    enable = true;
    drivers = with pkgs; [ gutenprint hplip epson-escpr ];
  };

  hardware.sane.enable = true;
}
