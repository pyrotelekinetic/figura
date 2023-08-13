{ config, lib, modulesPath, ... }: {

imports = [ (modulesPath + "/installer/scan/not-detected.nix") ];

boot = {
  initrd = {
    availableKernelModules = [
      "nvme"
      "xhci_pci"
      "usb_storage"
      "sd_mod"
    ];
    kernelModules = [ ];
  };
  kernelModules = [ "kvm-amd" ];
  extraModulePackages = [ ];
};

fileSystems = {
  "/" = {
    device = "/dev/disk/by-uuid/6915fc5c-2a8f-4d60-ad1f-4ee522816011";
    fsType = "ext4";
  };
  "/efi" = {
    device = "/dev/disk/by-uuid/1660-CB67";
    fsType = "vfat";
  };
};

swapDevices = [
  { device = "/dev/disk/by-uuid/b865335a-3e01-4de4-acda-47428a103f14"; }
];

# Enables DHCP on each ethernet and wireless interface. In case of scripted networking
# (the default) this is the recommended approach. When using systemd-networkd it's
# still possible to use this option, but it's recommended to use it in conjunction
# with explicit per-interface declarations with `networking.interfaces.<interface>.useDHCP`.
networking.useDHCP = lib.mkDefault true;
# networking.interfaces.enp3s0f4u1.useDHCP = lib.mkDefault true;
# networking.interfaces.wlp1s0.useDHCP = lib.mkDefault true;

nixpkgs.hostPlatform = lib.mkDefault "x86_64-linux";
hardware.cpu.amd.updateMicrocode = lib.mkDefault config.hardware.enableRedistributableFirmware;

}
