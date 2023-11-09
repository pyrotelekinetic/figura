{ config, lib, modulesPath, ... }@args: let
  cfg = config.head;
in {

options.head = with lib; {
  headless = mkEnableOption (mdDoc "remote only access");
  graphical = mkEnableOption (mdDoc "graphical user environment");
};

config = {
  #TODO: Move relevant config from ./default here
  assertions = [
    {
      assertion = cfg.graphical -> !cfg.headless;
      message = "system cannot be both headless and graphical";
    }
  ];
} // lib.mkIf cfg.headless (
  import (modulesPath + "/profiles/headless.nix") args
);

}
