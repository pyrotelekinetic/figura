{ config, ... }: {

imports = [
  ./clonehero.nix
  ./factorio.nix
];

services.syncthing = {
  enable = true;
  user = "cison";
  dataDir = config.users.users.cison.home + "/.config/syncthing";
  configDir = config.users.users.cison.home + "/.config/syncthing";
  key = config.sops.secrets."syncthing/key.pem".path;
  cert = config.sops.secrets."syncthing/cert.pem".path;
};

sops.secrets = {
  "syncthing/key.pem" = {
    sopsFile = ../secrets.yaml;
  };
  "syncthing/cert.pem" = {
    sopsFile = ../secrets.yaml;
  };
};

}
