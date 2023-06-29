{ lib, ... }: {

imports = [
  ./hardware.nix
  ./tlp.nix
];

# Don't want sshd on a portable machine that connects to lots of different networks
services.openssh.enable = lib.mkForce false;

# Control screen backlight, must be in video group
hardware.acpilight.enable = true;
users.users.cison.extraGroups = [ "video" ];

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

services.logind.lidSwitch = "hybrid-sleep";

}
