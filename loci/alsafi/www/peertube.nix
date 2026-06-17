{ config, ... }: {

sops.secrets."peertube" = {
  sopsFile = ../secrets.yaml;
  owner = config.services.peertube.user;
  restartUnits = [ "peertube.service" ];
};

services = {
  peertube = {
    enable = true;
    enableWebHttps = true;
    listenWeb = 443;
    settings = {
      listen.hostname = "127.0.0.1"; # configureNginx expects ipv4 loopback
    };
    secrets.secretsFile = config.sops.secrets."peertube".path;
    redis.createLocally = true;
    database.createLocally = true;
    localDomain = "tv.clover.isons.org";
    configureNginx = true;
  };

  nginx = {
    virtualHosts."tv.clover.isons.org" = {
      forceSSL = true;
      useACMEHost = "clover.isons.org";
    };
  };
};

}
