{ pkgs, ... }: {

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
};

hardware = {
  bluetooth = {
    enable = true;
    input.General.ClassicBondedOnly = false; # Enable wiimote pairing
  };
  keyboard.qmk.enable = true;
};

pyrosite.enable = true;

users.users.cison.packages = with pkgs; [
  lmms
  renoise
];

home-manager.users.cison.graphical.games = true;

}
