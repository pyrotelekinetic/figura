{ config, lib, inputs, ... }: {

options.remoteBuild = with lib; {
  enable = mkEnableOption (mdDoc "Whether to build on remote hosts");
  builder = mkEnableOption (mdDoc "Whether to enable remote builds on this host");
  speed = mkOption {
    type = types.int;
    default = 1;
    description = mdDoc "The speed factor of the machine.";
  };
};

config = let
  buildMachine = host: let cfg = host.config; in
    if cfg.remoteBuild.builder && cfg.system.name != config.system.name
    then {
      hostName = cfg.networking.hostName;
      systems = [ cfg.nixpkgs.system ] ++ cfg.nix.settings.extra-platforms or [ ];
      supportedFeatures = cfg.nix.settings.system-features;
      sshUser = "nix-ssh";
      speedFactor = cfg.remoteBuild.speed;
      protocol = "ssh-ng";
    }
    else null;
  substituter = host: let name = host.config.networking.hostName; in
    if host.config.system.name != config.system.name
    then "ssh-ng://nix-ssh@${name}?priority=30"
    else null;
  hosts = lib.attrValues inputs.self.nixosConfigurations;
  substituters = lib.remove null (map substituter hosts);
  cfg = config.remoteBuild;
in {
  nix = {
    settings = {
      extra-substituters = substituters;
      extra-trusted-substituters = substituters;
    };
    buildMachines = lib.remove null (map buildMachine hosts);
    distributedBuilds = cfg.enable;
    sshServe = {
      enable = true;
      protocol = "ssh-ng";
      write = true;
      keys = [
        "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIOeBhkBt7wNdRqBOSiKF+afBFR+QXCbSusk9UhTcCy+n altair"
        "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIEfm7DyzSbKkoTJKlVZiiS3XWedEkFSZZSAKH8ZgtW9q luna"
        "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIDvXEmYV/+fWAoJuxmJqXl13TqGEfvrPtkhMcvHplq1E sol"
        "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIKDDcOf+fZdGrU5vKMLzD1M4MaxC3zbWUzF5+NRkLJia vega"
      ];
    };
    settings.trusted-users = [ "nix-ssh" ];
  };
  assertions = [
    {
      assertion = cfg.builder -> config.services.openssh.enable;
      message = "the SSH daemon needs to be running in order to perform remote builds on this machine";
    }
  ];
};

}
