{ pkgs, ... }: {

services.minecraft-server = {
  enable = true;
  eula = true;
  openFirewall = true;
  declarative = true;
  serverProperties = {
    motd = "altair mc";
  };
};

}
