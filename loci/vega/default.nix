{ inputs, lib, pkgs, ... }: {

imports = [
  inputs.lanzaboote.nixosModules.lanzaboote
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

# For working with Secure Boot
environment.systemPackages = [ pkgs.sbctl ];

boot = {
  lanzaboote = {
    enable = true;
    pkiBundle = "/etc/secureboot";
  };
  loader = {
    systemd-boot = {
      enable = lib.mkForce false;
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
services.greetd.settings = {
  initial_session = {
    command = "/run/current-system/sw/bin/sway";
    user = "cison";
  };
  # Drop to shell if session is killed
  default_session = lib.mkForce {
    command = "/run/current-system/sw/bin/bash";
    user = "cison";
  };
};

services.logind.lidSwitch = "suspend-then-hibernate";

home-manager.users.cison.graphical.games = true;

}
