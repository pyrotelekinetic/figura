{ config, pkgs, ... }: {

networking.hosts = {
  "127.0.0.1" = [ "pyrosite" ];
};

services.httpd = {
  enable = true;
  user = "cison";
  virtualHosts."pyrosite".documentRoot = "/home/cison/projects/site/result/site";
};

}
