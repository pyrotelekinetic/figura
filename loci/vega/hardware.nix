{ config, lib, modulesPath, ... }: {

imports = [ (modulesPath + "/installer/scan/not-detected.nix") ];

boot = {
  initrd = {
    luks.devices.cryptlvm.device = "/dev/disk/by-uuid/ae6c7b6a-9763-4a76-8e13-69ac237ee4f0";
    availableKernelModules = [
      "nvme"
      "xhci_pci"
      "cryptd"
      "crypto-aes"
    ];
    kernelModules = [ "dm-snapshot" ];
  };
  kernelModules = [ "kvm-amd" ];
  extraModulePackages = [ ];
};

fileSystems = {
  "/" = {
    device = "/dev/disk/by-uuid/8fcf1c81-d18d-4168-ab6f-0536196b801b";
    fsType = "ext4";
  };
  "/efi" = {
    device = "/dev/disk/by-uuid/9493-B952";
    fsType = "vfat";
    options = [ "umask=0077" ];
  };
};

swapDevices = [
  { device = "/dev/disk/by-uuid/89b91b3c-40fd-4e03-8d35-f8f98dda3fc9"; }
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
