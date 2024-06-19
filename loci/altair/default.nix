{ lib, ... }: {

imports = [
  ./hardware.nix
  ./oci-script
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

remoteBuild.builder = true;
boot.binfmt.emulatedSystems = [ "aarch64-linux" ];

# altair is a vm... no SMART support
services.smartd.enable = lib.mkForce false;

system.stateVersion = "23.11";

}
