{ flake, config, lib, pkgs, ... }:

{
  services.udisks2.enable = true;

  services.playerctld.enable = true;

  home-manager.sharedModules = [{
    home.packages = with pkgs; [
      pcmanfm
      ffmpegthumbnailer
      notepad-next
      evince
      qimgv
      libreoffice
      udiskie
      libnotify
      networkmanagerapplet
      pavucontrol
      xarchiver
      xdg-utils
      gtk2  # For icon cache, see #20874.
      dconf
      ffmpeg-full
      git-filter-repo
      graphicsmagick
      imagemagick
      (python3.withPackages (p: with p; [ scipy geopy python-lsp-server requests pylint matplotlib tkinter beautifulsoup4 aiohttp humanize protobuf ]))
      flake.packages.${pkgs.system}.naps2
      hunspell
      hunspellDicts.pl_PL
      hunspellDicts.en_US
      nixd
    ];

    xdg.mimeApps.enable = true;
    xdg.mimeApps.defaultApplications = let
      webBrowser = ["firefox.desktop"];
      fileBrowser = ["pcmanfm.desktop"];
      textEditor = ["NotepadNext.desktop"];
      pdfViewer = ["org.gnome.Evince.desktop"];
      imageViewer = ["qimgv.desktop"];
      videoPlayer = ["mpv.desktop"];
      archiver = ["xarchiver.desktop"];
      wordLike = ["writer.desktop"];
      excelLike = ["calc.desktop"];
      powerpointLike = ["impress.desktop"];
    in {
      "inode/directory" = fileBrowser;
      "inode/mount-point" = fileBrowser;

      "x-scheme-handler/http" = webBrowser;
      "x-scheme-handler/https" = webBrowser;

      "x-scheme-handler/magnet" = webBrowser;  # Let a web app inside the web browser handle them.

      "text/plain" = textEditor;
      "application/rtf" = textEditor;

      "application/pdf" = pdfViewer;

      "application/msword" = wordLike;
      "application/vnd.openxmlformats-officedocument.wordprocessingml.document" = wordLike;
      "application/vnd.oasis.opendocument.text" = wordLike;
      "application/vnd.oasis.opendocument.text-flat-xml" = wordLike;

      "application/vnd.ms-excel" = excelLike;
      "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet" = excelLike;
      "application/vnd.oasis.opendocument.spreadsheet" = excelLike;
      "application/vnd.oasis.opendocument.spreadsheet-flat-xml" = excelLike;

      "application/vnd.ms-powerpoint" = powerpointLike;
      "application/vnd.openxmlformats-officedocument.presentationml.presentation" = powerpointLike;
      "application/vnd.oasis.opendocument.presentation" = powerpointLike;
      "application/vnd.oasis.opendocument.presentation-flat-xml" = powerpointLike;

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

      "application/zip" = archiver;
      "application/vnd.rar" = archiver;
      "application/x-7z-compressed" = archiver;
      "application/x-tar" = archiver;
      "application/x-compressed-tar" = archiver;
      "application/x-bzip2-compressed-tar" = archiver;
    };
  }];

  services.printing = {
    enable = true;
    drivers = with pkgs; [ gutenprint hplip epson-escpr ];
  };

  hardware.sane.enable = true;
}
