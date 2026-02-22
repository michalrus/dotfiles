{pkgs, ...}: {
  home = {
    packages = [pkgs.alacritty];
    sessionVariables.TERMINAL = "alacritty";
    file.".config/alacritty/alacritty.toml".source = ./alacritty.toml;
  };
}
