{ lib, modulesPath, ... }: {

imports = [
  (modulesPath + "/installer/scan/not-detected.nix")
  (modulesPath + "/profiles/qemu-guest.nix")
];

fileSystems = {
  "/" = {
    device = "/dev/disk/by-uuid/ece712d9-685e-4d14-b4ca-26c2617141e4";
    fsType = "ext4";
  };
  "/efi" = {
    device = "/dev/disk/by-uuid/79D0-5304";
    fsType = "vfat";
  };
};

swapDevices = [
  { device = "/dev/disk/by-uuid/3dd42a93-7ace-4fd7-856c-74099341c257"; }
];

networking.useDHCP = lib.mkDefault true;

nixpkgs.hostPlatform = lib.mkDefault "x86_64-linux";

}
