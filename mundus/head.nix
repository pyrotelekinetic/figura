{ config, pkgs, lib, modulesPath, ... }@args: let
  cfg = config.head;
in {

options.head = with lib; {
  headless = mkEnableOption (mdDoc "remote only access");
  graphical = mkEnableOption (mdDoc "graphical user environment");
};

config = lib.mkMerge [
  (
    lib.mkIf cfg.headless (
      import (modulesPath + "/profiles/headless.nix") args
    ) // {
      # screen is nice for leaving a session running while disconnecting ssh
      environment.systemPackages = [ pkgs.screen ];
      # Generating man cache is really slow, I can just use it from local system
      documentation.man.generateCaches = lib.mkForce false;
    }
  )
  {
    assertions = [
      {
        assertion = cfg.graphical -> !cfg.headless;
        message = "system cannot be both headless and graphical";
      }
    ];
  }
];

}
