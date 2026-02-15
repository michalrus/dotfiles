{...}: {
  # Because of this insanity o_O’ → <https://github.com/NixOS/nixpkgs/pull/16021>
  services.logind.settings.Login = {
    KillUserProcesses = true;
    UserStopDelaySec = 0;
  };
}
