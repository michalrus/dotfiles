{
  services.openssh = {
    enable = true;
    settings = {
      PasswordAuthentication = false;
      KbdInteractiveAuthentication = false;
    };
  };

  users = {
    users.root = {
      dotfiles-old.profiles = [ "base" ];
      openssh.authorizedKeys.keyFiles = [ ../../../../dotfiles/michalrus/base/.ssh/authorized_keys.d/michalrus_notebook.pub ];
      hashedPassword = "$6$nYYt5qEy6UEZDbtV$YFbJ7iKb.o6cxPLDJUaU2IJOpf1W4UKlssB3Fr38bX8M11qPWbKfdafi9ri0CSd9PayDrFJNAr9RWdiyQzuy6/";
    };

    users.m = {
      isNormalUser = true;
      openssh.authorizedKeys.keyFiles = [ ../../../../dotfiles/michalrus/base/.ssh/authorized_keys.d/michalrus_notebook.pub ];
      description = "Michal Rus";
      extraGroups = [ "wheel" ];
      hashedPassword = "$6$21U4zBIFvrW/htmo$s33ze63x3AbM5vjIL8lxo84beXBxHTI/hqp1La2TRw70KLKDMto6mzM6D2oNMGLUoBbof9zuwip9kBc7xCm4e0";
      dotfiles-old.profiles = [ "base" "michalrus/base" "michalrus/personal" ];
    };
  };
}
