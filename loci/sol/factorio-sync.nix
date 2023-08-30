{ config, ... }: {

services.syncthing = {
  enable = true;
  user = "cison";
  dataDir = "/home/cison";
  key = config.sops.secrets."syncthing/key.pem".path;
  cert = config.sops.secrets."syncthing/cert.pem".path;
  settings = {
    devices.vault.id =
      "3PC5XF2-ELUCF4L-F4YXZWD-KHUFWTJ-KLRK35B-FQMYYO3-A6UN4JR-NQPPTAY";
    folders.Factorio-Saves = {
      path = "~/.factorio/saves/sync";
      id = "xqg4g-nykzf";
      devices = [ "vault" ];
    };
  };
};

sops.secrets = {
  "syncthing/key.pem" = {
    sopsFile = ./secrets.yaml;
  };
  "syncthing/cert.pem" = {
    sopsFile = ./secrets.yaml;
  };
};

}
