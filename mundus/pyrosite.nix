{ config, lib, pkgs, ... }: let
  cfg = config.pyrosite;
in with lib; {

options.pyrosite = {
  enable = mkEnableOption (mdDoc "local pyrosite instance");
  path = mkOption {
    type = types.path;
    default = "${config.users.users.cison.home}/projects/site/result/site";
    description = mdDoc "The output directory of the flake build..";
  };
};

config = mkIf cfg.enable {
  networking.hosts = {
    "127.0.0.1" = [ "pyrosite" ];
  };

  services.httpd = {
    enable = true;
    user = "cison";
    virtualHosts."pyrosite".documentRoot = "${cfg.path}";
  };
};

}
