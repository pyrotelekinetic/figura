{ config, pkgs, inputs, ... }: {

imports = [ ./invidious.nix ];

sops.secrets."duckdns.token".sopsFile = ./secrets.yaml;

services = {
  duckdns = {
    enable = true;
    tokenFile = config.sops.secrets."duckdns.token".path;
    domains = [ "cloverp" ];
  };

  nginx = {
    enable = true;
    virtualHosts."cloverp.duckdns.org" = {
      root = inputs.pyrosite.packages.${pkgs.system}.default + "/site";
      forceSSL = true;
      enableACME = true;
    };
  };
};

security.acme = {
  defaults = {
    email = "carter+acme@isons.org";
    # just setting dnsProvider doesn't seem to work
    extraLegoFlags = [ "--dns" "duckdns" ];
    dnsProvider = "duckdns";
    credentialFiles."DUCKDNS_TOKEN_FILE" = config.sops.secrets."duckdns.token".path;
  };
  acceptTerms = true;
};

networking.firewall.allowedTCPPorts = [ 80 443 ];

}
