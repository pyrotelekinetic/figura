{ inputs, lib, ... }: {

imports = [ ./hardware.nix ];

head.headless = true;

boot = {
  loader = {
    systemd-boot = {
      enable = true;
      configurationLimit = 1; # I can't make a selection at boot anyway
      editor = false;
    };
    efi = {
      efiSysMountPoint = "/efi";
      canTouchEfiVariables = true;
    };
  };
};

# Generating man cache is really slow, I don't really need it anyway
documentation.man.generateCaches = lib.mkForce false;

system.stateVersion = "23.11";

}
