{ inputs, lib, pkgs, ... }: {

imports = [
  ./hardware.nix
  ./wireguard.nix
  inputs.nixos-hardware.nixosModules.raspberry-pi-4
];

head.headless = true;

nixpkgs.overlays = [
  # Some modules are missing for aarch64
  (_self: super: {
    makeModulesClosure = x:
      super.makeModulesClosure (x // { allowMissing = true; });
  })
];

# Use different port for external ssh
services.openssh.ports = [ 2885 ];

# luna doesn't have any SMART enabled devices
services.smartd.enable = lib.mkForce false;

home-manager.users.cison.home.packages = [ pkgs.wakeonlan ];

# Power on sol with 'wakesol'
home-manager.users.cison.home.shellAliases.wakesol = "wakeonlan d4:5d:64:d4:c8:4f";

system.stateVersion = "23.11";

}
