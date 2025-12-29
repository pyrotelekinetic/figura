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
    virtualHosts."calibre.cloverp.duckdns.org" = {
      forceSSL = true;
      enableACME = true;
      locations."/" = {
        proxyPass = "http://[::1]:" + toString port;
        recommendedProxySettings = true;
      };
    };
  };
};

}
