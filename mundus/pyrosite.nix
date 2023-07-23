{ config, lib, ... }: with lib; {

options.pyrosite = {
  enable = mkEnableOption (mdDoc "webserver for working on pyrosite");
  path = mkOption {
    type = types.path;
    default = "/home/cison/projects/site/result/site";
    description = mdDoc "The root directory of pyrosite.";
  };
};

config = mkIf config.pyrosite.enable {
  networking.hosts."127.0.0.1" = [ "pyrosite" ];

  services.httpd = {
    enable = true;
    user = "cison";
    virtualHosts."pyrosite".documentRoot = config.pyrosite.path;
  };
};

}
