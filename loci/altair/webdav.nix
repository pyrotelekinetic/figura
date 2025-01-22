{ config, ... }: {

sops.secrets."webdavEnv" = {
#  owner = config.services.webdav.user;
  sopsFile = ./secrets.yaml;
  restartUnits = [ "webdav.service" ];
};

services.webdav = {
  enable = true;
  settings = {
    address = "0.0.0.0";
    port = 8088;
    directory = "/srv/seedvault";
    users = [ {
      username = "{env}USERNAME";
      password = "{env}PASSWORD";
      permissions = "CRUD";
    } ];
  };
};

networking.firewall.allowedTCPPorts = [ 8088 ];

systemd.services.webdav.serviceConfig.EnvironmentFile = config.sops.secrets."webdavEnv".path;

}
