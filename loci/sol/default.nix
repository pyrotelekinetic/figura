{ ... }: {

imports = [
  ./hardware.nix
  ./sway.nix
];

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
};

hardware.keyboard.qmk.enable = true;

pyrosite.enable = true;

services.syncthing.enable = true;
home-manager.users.cison.services.syncthing.enable = true;

home-manager.users.cison = {
  graphical = {
    enable = true;
    games = true;
  };
};

}
