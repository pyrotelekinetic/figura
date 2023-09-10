{ inputs, lib, pkgs, ... }: {

imports = [
  ./hardware.nix
  inputs.nixos-hardware.nixosModules.raspberry-pi-4
];

nixpkgs.overlays = [
  # Some modules are missing for aarch64
  (self: super: {
    makeModulesClosure = x:
      super.makeModulesClosure (x // { allowMissing = true; });
  })
];

# Use different port for external ssh
services.openssh.ports = [ 2885 ];

# Generating man cache is really slow, I don't really need it anyway
documentation.man.generateCaches = lib.mkForce false;

# luna doesn't have any SMART enabled devices
services.smartd.enable = lib.mkForce false;

home-manager.users.cison.home.packages = [ pkgs.screen ];

system.stateVersion = "23.11";

}
