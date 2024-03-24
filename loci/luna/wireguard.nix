{ pkgs, config, lib, ... }: {

environment.systemPackages = [ pkgs.wireguard-tools ];

networking.firewall.allowedUDPPorts = [ 24245 ];
#networkingfirewall.interfaces.wg0.allowedUDPPorts = [ 24245 ];
systemd.network = {
  enable = true;
  netdevs."50-wg0" = {
    netdevConfig = {
      Name = "wg0";
      Kind = "wireguard";
    };
    wireguardConfig = {
      PrivateKeyFile = "/etc/wireguard/secret.key";
      ListenPort = 24245;
    };
    wireguardPeers = [
      { #hartley
        PublicKey = "wCz3fbgUE2fbhJ1jYznEsJKfyj8zQJn3/qlppEb/FXU=";
          AllowedIPs = [ "10.0.0.0/0" ];
      }
    ];
  };
  networks.wg0 = {
    matchConfig.Name = "wg0";
    address = [ "10.0.0.1/24" ];
    networkConfig = {
      IPMasquerade = "ipv4";
      IPv4Forwarding = true;
    };
  };
};

}
