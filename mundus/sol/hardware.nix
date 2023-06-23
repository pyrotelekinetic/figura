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
  };
  "/data" = {
    label = "data";
    fsType = "ext4";
    noCheck = true;
  };
};

swapDevices = [
  {
    device = "/var/swap";
    size = 2048;
  }
];

# Enables DHCP on each ethernet and wireless interface. In case of scripted networking
# (the default) this is the recommended approach. When using systemd-networkd it's
# still possible to use this option, but it's recommended to use it in conjunction
# with explicit per-interface declarations with `networking.interfaces.<interface>.useDHCP`.
networking.useDHCP = lib.mkDefault true;
# networking.interfaces.enp3s0.useDHCP = lib.mkDefault true;

nixpkgs.hostPlatform = lib.mkDefault "x86_64-linux";
hardware.cpu.amd.updateMicrocode = lib.mkDefault config.hardware.enableRedistributableFirmware;

}
