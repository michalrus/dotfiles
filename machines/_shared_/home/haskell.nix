{ config, flake, pkgs, lib, ... }:

{

  home.file.".ghci".text = ''
    :set prompt "λ> "
    :set +s
    :def hoogle \s -> return $ ":! hoogle search -cl --count=15 \"" ++ s ++ "\""
    :def doc    \s -> return $ ":! hoogle search -cl --info \"" ++ s ++ "\""
  '';

}
