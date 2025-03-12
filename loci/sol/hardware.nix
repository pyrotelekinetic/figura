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
    device = "/dev/disk/by-uuid/533e2a79-661f-45ad-9352-8e1bb379763f";
    fsType = "ext4";
  };
  "/efi" = {
    device = "/dev/disk/by-uuid/6788-8268";
    fsType = "vfat";
    options = [ "umask=0077" ];
  };
  "/data" = {
    device = "/dev/disk/by-uuid/0f3118ff-215e-4eee-8d88-4688f467daa3";
    fsType = "ext4";
  };
};

swapDevices = [
  { device = "/dev/disk/by-uuid/2712e6ac-e593-4032-952d-4a8d78df2c26"; }
];

networking.interfaces.enp3s0.useDHCP = lib.mkDefault true;

nixpkgs.hostPlatform = lib.mkDefault "x86_64-linux";
hardware.cpu.amd.updateMicrocode = lib.mkDefault config.hardware.enableRedistributableFirmware;

}
