{ inputs, lib, ... }: {

imports = [
  ./hardware.nix
  inputs.nixos-hardware.nixosModules.raspberry-pi-4
];

nixpkgs.overlays = [
  (self: super: {
    makeModulesClosure = x:
      super.makeModulesClosure (x // { allowMissing = true; });
  })
];

documentation.man.generateCaches = lib.mkForce false;

system.stateVersion = "23.11";

}
