{ pkgs, config, lib, ... }: let
  cfg = config.wireguard;
in {

options.wireguard = {
  enable = lib.mkEnableOption (lib.mdDoc "wireguard vpn");
};

config = lib.mkIf cfg.enable {
  systemd.network = {
    enable = true;
    netdevs.wg0 = {
      netdevConfig = {
        Name = "wg0";
        Kind = "wireguard";
      };
      wireguardConfig = {
        PrivateKeyFile = "/etc/wireguard/secret.key";
        ListenPort = 24245;
      };
      wireguardPeers = if config.system.name == "sol"
        then [ {
          wireguardPeerConfig = {
            PublicKey = "JJj2m7jkYu3k7meMcKhMqCk1b3kPo8DSwMSBjRZd7Wc=";
            AllowedIPs = "10.0.0.1/32";
            Endpoint = "luna:24245";
          };
        } ]
        else [ {
          wireguardPeerConfig = {
            PublicKey = "1mjBzyoL6sCs7fT+19FThUryD3mmgv4SLs04ZqTbHhI=";
            AllowedIPs = "10.0.0.2/32";
            Endpoint = "sol:24245";
          };
        } ];
    };
    networks.wg0 = {
      name = "wg0";
      address = if config.system.name == "sol"
        then [ "10.0.0.2/24" ]
        else [ "10.0.0.1/24" ];
      linkConfig.RequiredForOnline = false;
    };
  };
  #networking.interfaces.wg0.useDHCP = true;
  networking.firewall.interfaces.wg0.allowedUDPPorts = [ 24245 ];
  environment.systemPackages = [ pkgs.wireguard-tools ];
};

}
