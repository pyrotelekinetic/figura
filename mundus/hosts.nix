{ lib, ... }: let
  ips = {
    wan = "184.179.188.130";
    gate = "192.168.1.1";
    altair = "129.146.87.209";
    luna = "192.168.1.2";
    sol = "192.168.1.8";
    halley = "192.168.1.9";
    vega = "192.168.1.10";
  };
in {

networking.hosts = lib.concatMapAttrs (x: y: { ${y} = [ x ]; }) ips;

programs.ssh = {
  extraConfig = with ips; ''
    Host altair
      HostName ${altair}
    Host luna-
      Port 2885
      HostName ${wan}
      ProxyJump none
    Host luna
      HostName ${luna}
    Host sol
      HostName ${sol}
    Host sol-
      HostName ${sol}
      ProxyJump luna-
    Host vega
      HostName ${vega}
    Host vega-
      HostName ${vega}
      ProxyJump luna-
    Host github
      HostName github.com
      User git
    Host codeberg
      HostName codeberg.org
      User git
  '';
  knownHosts = {
    altair = {
      extraHostNames = [ ips.altair ];
      publicKey = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIOeBhkBt7wNdRqBOSiKF+afBFR+QXCbSusk9UhTcCy+n";
    };
    luna = {
      extraHostNames = [
        "luna-"
        ips.luna
        ips.wan
      ];
      publicKey = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIPyiB/W/vRogqYWG9GYJXDpgfZKQhctysjTA8Xa0cg4j";
    };
    sol = {
      extraHostNames = [
        "sol-"
        ips.sol
      ];
      publicKey = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAICMD/mJgXujNZnz+uFO9EUDABTZqhQpVcenYtgrZvzgN";
    };
    vega = {
      extraHostNames = [
       "vega-"
        ips.vega
      ];
      publicKey = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIEzg0L3GO8bRTEqR2tgoNbMTtWlkV64R0LIlyFYgxEqY";
    };
    github = {
      extraHostNames = [ "github.com" ];
      publicKey = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIOMqqnkVzrm0SdG6UOoqKLsabgH5C9okWi0dh2l9GKJl";
    };
    codeberg = {
      extraHostNames = [ "codeberg.org" ];
      publicKey = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIIVIC02vnjFyL+I4RHfvIGNtOgJMe769VTF1VR4EB3ZB";
    };
  };
};

}
