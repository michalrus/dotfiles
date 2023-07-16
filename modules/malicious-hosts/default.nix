{ danPollock }:

{ ... }:

{
  networking.extraHosts = ''
    ### Taken from ${danPollock} :
    ${builtins.readFile danPollock}
  '';
}
