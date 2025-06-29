{ lib, ... }: {

imports = [
  ./hardware.nix
  ./disko.nix
  ./www.nix
];

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

# alsafi is a vm... no SMART support
services.smartd.enable = lib.mkForce false;

users.mutableUsers = false;
users.users.cison.initialHashedPassword = lib.mkForce null;
# Passwordless, so allow polkit actions without auth as well
security.polkit.extraConfig = ''
  polkit.addRule(function(action, subject) {
    if (subject.isInGroup("wheel")) return polkit.Result.YES;
  });
'';

system.stateVersion = "24.11";

}
