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

services.logind.lidSwitch = "suspend-then-hibernate";

home-manager.users.cison.graphical.games = true;

}
