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

# altair is a vm... no SMART support
services.smartd.enable = lib.mkForce false;

system.stateVersion = "23.11";

}
