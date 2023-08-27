{ lib, ... }: {

imports = [
  ./hardware.nix
  ./tlp.nix
  ./sway.nix
];

system.stateVersion = "23.05";

# Don't want sshd on a portable machine that connects to lots of different networks
services.openssh.enable = lib.mkForce false;

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
  plymouth.enable = true;
};

services.logind.lidSwitch = "suspend-then-hibernate";

home-manager.users.cison = {
  graphical = {
    enable = true;
    games = true;
  };
};

}
