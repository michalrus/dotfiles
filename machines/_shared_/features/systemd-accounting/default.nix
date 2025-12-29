{ ... }:

{
  systemd.settings.Manager = {
    DefaultCPUAccounting = true;
    DefaultBlockIOAccounting = true;
    DefaultMemoryAccounting = true;
    DefaultTasksAccounting = true;
    DefaultIPAccounting = true;
  };
}
