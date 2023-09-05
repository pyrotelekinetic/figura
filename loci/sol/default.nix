{ pkgs, ... }: {

imports = [
  ./hardware.nix
  ./sway.nix
  ./factorio-sync.nix
];

system.stateVersion = "22.11";

networking = {
  hosts."192.168.1.2" = [ "luna" ];
  interfaces.enp3s0.wakeOnLan.enable = true;
};

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
  binfmt.emulatedSystems = [ "aarch64-linux" ];
};

hardware = {
  bluetooth.enable = true;
  keyboard.qmk.enable = true;
};

pyrosite.enable = true;

home-manager.users.cison = {
  graphical = {
    enable = true;
    games = true;
  };
  home.packages = with pkgs; [ lmms ];
};

}
