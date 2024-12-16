let

  # people
  michalrus = [
    # personal GPG:
    "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAABAQCbtBIiVRih4vlbX8ciQELf9wST0tYIygNfPxNjtm1IElpOKVT+j62hPui+d0VELKuxJcyo3tY9nf2zUYUG3PG7IWjyiHi6FyOasUQLzJrXBRj5dNsPr+SYXAyL1jsTbvbfiIUkfPAPuv5Tf/tg/lAdTriTy73V5sN7vtX+MH2k8n4agE6fhj2FAhiSwI4MAZJmIsNB2X+1GZVLZlggpN7tkkfjFWE5nCvlR+/lA6e0wl9ZCzTas112fTTBUk64wd1U7vlv1+nr7YgVAqyAQR/w7VCe0z3hrwIwxCOdW3nN19dW2gCQ7gKrZbDfaU3/OqURTNq9zwdET/mNM7unF4sX"
    # ~/.password-store – see `pass show Work/agenix-key`
    "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIByU3xFWPma65VksaDp297Roc/KECoGznwieLWGO1qxY"
  ];

  # machines
  michalrus_com = ["ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIHAq3I/JNJoWzLYn6/KSWiG3IfFthdeMGuWpRm0OSM4I"];
  lenovo_x1 = ["ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAICMEf2VphaaVMFHAnt09xyMjpnIdxaECdvTZl/i4R3s5"];
  macbook = ["ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAILZEeBucsKC/jKItBn8MQxSBSUiO2oMCHn5u5iSxI7Ac"];
  macbook-nixos = ["ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIO4xwYfoldXil0aOHLOzSugiLKVqh/M7Hmi6T2Awv7k0"];
  dell-home-server = ["ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIHIF7BIoIAPX1gBffDvuKoHHXrHRbgJdDze6DhH97N5B"];
  monstrum = ["ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIAII+f73fJcgbVVbfMI+lAzOk582TGp6t7O3jFxk60B8"];

in {
  "openproject_key_base.age".publicKeys = michalrus ++ michalrus_com ++ monstrum;
  "smtp_scripts_michalrus_com.age".publicKeys = michalrus ++ michalrus_com ++ monstrum;
  "wireguard_monstrum.age".publicKeys = michalrus ++ monstrum;
  "wireguard_michalrus_com.age".publicKeys = michalrus ++ michalrus_com;
  "transmission_rpc_password.age".publicKeys = michalrus ++ monstrum;
  "transmission_rpc_password_nginx.age".publicKeys = michalrus ++ monstrum;

  "ssh-key-personal-git-annex.age".publicKeys = michalrus ++ lenovo_x1 ++ macbook ++ macbook-nixos;
  "ssh-config-work-devx.age".publicKeys = michalrus ++ lenovo_x1 ++ macbook ++ macbook-nixos;
  "ssh-config-work-iog.age".publicKeys = michalrus ++ lenovo_x1 ++ macbook ++ macbook-nixos;
  "ssh-config-work-lace.age".publicKeys = michalrus ++ lenovo_x1 ++ macbook ++ macbook-nixos;
  "ssh-known_hosts-work-devx.age".publicKeys = michalrus ++ lenovo_x1 ++ macbook ++ macbook-nixos;
  "ssh-known_hosts-work-iog.age".publicKeys = michalrus ++ lenovo_x1 ++ macbook ++ macbook-nixos;
  "ssh-known_hosts-work-lace.age".publicKeys = michalrus ++ lenovo_x1 ++ macbook ++ macbook-nixos;
}
