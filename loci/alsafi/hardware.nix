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

networking.interfaces.enp0s6.useDHCP = lib.mkDefault true;

nixpkgs.hostPlatform = lib.mkDefault "aarch64-linux";

}
