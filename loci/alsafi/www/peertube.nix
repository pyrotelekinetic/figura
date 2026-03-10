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
      listen.hostname = "::1";
    };
    secrets.secretsFile = config.sops.secrets."peertube".path;
    redis.createLocally = true;
    database.createLocally = true;
    localDomain = "tv.cloverp.duckdns.org";
    configureNginx = false;
  };

  nginx = {
    virtualHosts."tv.cloverp.duckdns.org" = {
      forceSSL = true;
      useACMEHost = "sub-cloverp.duckdns.org";
      locations."/" = {
        proxyPass = "http://[::1]:" + toString config.services.peertube.listenHttp;
        recommendedProxySettings = true;
      };
    };
  };
};

}
