{ config, lib, modulesPath, ... }: {

imports = [ (modulesPath + "/installer/scan/not-detected.nix") ];

boot = {
  initrd = {
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

networking.interfaces.wlp1s0.useDHCP = lib.mkDefault true;

nixpkgs.hostPlatform = lib.mkDefault "x86_64-linux";
hardware.cpu.amd.updateMicrocode = lib.mkDefault config.hardware.enableRedistributableFirmware;

}
