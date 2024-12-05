{ pkgs, config, ... }: let
  hid-wiimote-plus = config.boot.kernelPackages.callPackage ./hid-wiimote-plus.nix {};
in {

imports = [
  ./hardware.nix
  ./sway.nix
  ./sync
  ./obs.nix
  ./boot-chime.nix
  ./dualsense.nix
];

head.graphical = true;

system.stateVersion = "22.11";

networking.interfaces.enp3s0.wakeOnLan.enable = true;

remoteBuild.builder = true;

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
  binfmt.emulatedSystems = [ "aarch64-linux" ];
  extraModulePackages = [ hid-wiimote-plus ];
};

services.udev.packages = [ hid-wiimote-plus.udev ];

hardware = {
  bluetooth = {
    enable = true;
    input.General.ClassicBondedOnly = false; # Enable wiimote pairing
  };
  keyboard.qmk.enable = true;
};

systemd.user.services.auto-mute-disable = {
  description = "Disable alsa auto mute mode";
  wantedBy = [ "pipewire.service" ];
  after = [ "pipewire.service" ];
  path = [ pkgs.alsa-utils ];
  script = ''amixer -c1 sset "Auto-Mute Mode" Disabled'';
};

pyrosite.enable = true;

users.users.cison = {
  packages = with pkgs; [
    lmms
    renoise
  ];
  extraGroups = [ "adbusers" ];
};

programs.adb.enable = true;

home-manager.users.cison.graphical.games = true;

}
