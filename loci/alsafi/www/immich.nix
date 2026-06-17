{ config, ... }: let
  cfg = config.services.immich;
in {

services = {
  immich = {
    enable = true;
    host = "::1";
    openFirewall = false;
  };

  nginx = {
    virtualHosts."pics.clover.isons.org" = {
      forceSSL = true;
      useACMEHost = "clover.isons.org";
      locations."/" = {
        proxyPass = "http://[::1]:" + toString cfg.port;
        recommendedProxySettings = true;
        proxyWebsockets = true;
      };
      extraConfig = ''
        # From <https://docs.immich.app/administration/reverse-proxy>

        # allow large file uploads
        client_max_body_size 50000M;

        # disable buffering uploads to prevent OOM on reverse proxy server and make uploads twice as fast (no pause)
        proxy_request_buffering off;

        # increase body buffer to avoid limiting upload speed
        client_body_buffer_size 1024k;

        # Set headers
        proxy_set_header Host              $host;
        proxy_set_header X-Real-IP         $remote_addr;
        proxy_set_header X-Forwarded-For   $proxy_add_x_forwarded_for;
        proxy_set_header X-Forwarded-Proto $scheme;

        # enable websockets: http://nginx.org/en/docs/http/websocket.html
        proxy_http_version 1.1;
        proxy_redirect     off;

        # set timeout
        proxy_read_timeout 600s;
        proxy_send_timeout 600s;
        send_timeout       600s;
      '';
    };
  };
};

}
