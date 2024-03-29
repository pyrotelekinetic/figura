{ config, lib, modulesPath, ... }: {

imports = [ (modulesPath + "/installer/scan/not-detected.nix") ];

boot = {
  initrd = {
    availableKernelModules = [
      "xhci_pci"
      "ahci"
      "nvme"
      "usbhid"
      "sd_mod"
      "sr_mod"
    ];
    kernelModules = [ ];
  };
  kernelModules = [ "kvm-amd" ];
  extraModulePackages = [ ];
};

fileSystems = {
  "/" = {
    device = "/dev/disk/by-uuid/5a16f4f3-b01d-469d-9d10-087ec6cdd9e0";
    fsType = "ext4";
  };
  "/efi" = {
    device = "/dev/disk/by-uuid/004D-096C";
    fsType = "vfat";
    options = [ "umask=0077" ];
  };
  "/data" = {
    device = "/dev/disk/by-uuid/0f3118ff-215e-4eee-8d88-4688f467daa3";
    fsType = "ext4";
  };
};

swapDevices = [
  { device = "/dev/disk/by-uuid/36c68f75-ecb4-4b5e-9b2e-789607d5b694"; }
];

networking.interfaces.enp3s0.useDHCP = lib.mkDefault true;

nixpkgs.hostPlatform = lib.mkDefault "x86_64-linux";
hardware.cpu.amd.updateMicrocode = lib.mkDefault config.hardware.enableRedistributableFirmware;

}
