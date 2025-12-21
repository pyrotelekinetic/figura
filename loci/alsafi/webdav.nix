{ config, ... }: let
  port = 8689;
in {

services = {
  webdav = {
    enable = true;
    environmentFile = config.sops.secrets."webdav.env".path;
    settings = {
      address = "[::1]";
      inherit port;
      directory = "/srv/webdav/seedvault";
      permissions = "none";
      users = [ {
        username = "{env}USERNAME";
        password = "{env}PASSWORD";
        permissions = "CRUD";
      } ];
    };
  };

  nginx.virtualHosts."webdav.cloverp.duckdns.org" = {
    forceSSL = true;
    enableACME = true;
    locations."/" = {
      proxyPass = "http://[::1]:" + toString port;
      extraConfig = ''
        # copied from upstream docs:
        # <https://github.com/hacdias/webdav/blob/010ca576fbc6795196183505dcedeb24b3ec6cc8/README.md#nginx-configuration-example>

        proxy_set_header X-Real-IP $remote_addr;
        proxy_set_header REMOTE-HOST $remote_addr;
        proxy_set_header X-Forwarded-For $proxy_add_x_forwarded_for;
        proxy_set_header Host $host;
        proxy_redirect off;

        # Ensure COPY and MOVE commands work. Change https://example.com to the
        # correct address where the WebDAV server will be deployed at.
        set $dest $http_destination;
        if ($http_destination ~ "^https://webdav.cloverp.duckdns.org(?<path>(.+))") {
          set $dest /$path;
        }
        proxy_set_header Destination $dest;
      '';
    };
  };
};

sops.secrets."webdav.env".sopsFile = ./secrets.yaml;

}
