{ lib, modulesPath, ... }: {

imports = [ (modulesPath + "/installer/scan/not-detected.nix") ];

boot = {
  initrd = {
    availableKernelModules = [ "xhci_pci" "usb_storage" ];
    kernelModules = [ ];
  };
  kernelModules = [ ];
  extraModulePackages = [ ];
};

fileSystems."/" =
  {
    device = "/dev/disk/by-uuid/8452721d-6394-48d0-97d3-20f6f2d0d3c0";
    fsType = "ext4";
  };

swapDevices = [ ];

networking.useDHCP = lib.mkDefault true;

nixpkgs.hostPlatform = lib.mkDefault "aarch64-linux";
powerManagement.cpuFreqGovernor = lib.mkDefault "ondemand";

}
