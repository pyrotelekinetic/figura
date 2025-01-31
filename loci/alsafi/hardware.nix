{ lib, modulesPath, ... }: {

imports = [
  (modulesPath + "/profiles/qemu-guest.nix")
];

boot = {
  initrd = {
    availableKernelModules = [ "xhci_pci" "virtio_scsi" ];
    kernelModules = [ ];
  };
  kernelModules = [ ];
  extraModulePackages = [ ];
};

fileSystems = {
  "/" = {
    device = "/dev/disk/by-uuid/7e1d9061-75b9-43bf-be4c-2de9065cbebb";
    fsType = "ext4";
  };
  "/efi" = {
    device = "/dev/disk/by-uuid/7162-8E8D";
    fsType = "vfat";
    options = [ "umask=0077" "fmask=0022" "dmask=0022" ];
  };
};

swapDevices = [
  { device = "/dev/disk/by-uuid/79f18f70-66be-479e-afc7-0e95e17037e4"; }
];

networking.interfaces.enp0s6.useDHCP = lib.mkDefault true;

nixpkgs.hostPlatform = lib.mkDefault "aarch64-linux";

}
