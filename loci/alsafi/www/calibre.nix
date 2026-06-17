{ ... }: let
  port = 8391;
in {

services = {
  calibre-server = {
    enable = true;
    host = "::1";
    inherit port;
    openFirewall = false;
    libraries = [ "/srv/calibre/library" ];
    auth = {
      enable = true;
      userDb = "/srv/calibre/users.sqlite";
      mode = "basic";
    };
  };

  nginx = {
    clientMaxBodySize = "1G"; # for uploading big books
    virtualHosts."calibre.clover.isons.org" = {
      forceSSL = true;
      useACMEHost = "clover.isons.org";
      locations."/" = {
        proxyPass = "http://[::1]:" + toString port;
        recommendedProxySettings = true;
      };
    };
  };
};

}
