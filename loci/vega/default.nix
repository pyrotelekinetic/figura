{ lib, ... }: {

imports = [
  ./hardware.nix
  ./tlp.nix
  ./sway.nix
];

head.graphical = true;

system.stateVersion = "23.05";

# Control screen backlight, must be in video group
hardware.acpilight.enable = true;
users.users.cison.extraGroups = [ "video" ];

hardware.bluetooth.enable = true;

networking.networkmanager.enable = true;

boot = {
  loader = {
    systemd-boot = {
      enable = true;
      configurationLimit = 5;
      editor = false;
    };
    efi = {
      efiSysMountPoint = "/efi";
      canTouchEfiVariables = true;
    };
  };
};

# This bitch is secured with fde, we can autologin
security.sudo.wheelNeedsPassword = false;
users.mutableUsers = false;
users.users.cison.initialHashedPassword = lib.mkForce null;
services.getty.autologinUser = "cison";

services.logind.lidSwitch = "suspend-then-hibernate";

home-manager.users.cison.graphical.games = true;

}
