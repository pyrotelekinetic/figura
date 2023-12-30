inputs: host: with inputs;

nixpkgs.lib.nixosSystem {
  specialArgs = { inherit inputs; };
  modules = [
    ./mundus
    "${self}/loci/${host}"
    ./domus
    sops-nix.nixosModules.sops
    pinputs.nixosModules.default
    {
      system.configurationRevision = self.rev or "DIRTY";
      networking.hostName = host;
      pins = { inherit inputs; };
    }
    home-manager.nixosModules.home-manager {
      home-manager = {
        extraSpecialArgs = { inherit (pyroscheme.lib) colors; };
        useGlobalPkgs = true;
        useUserPackages = true;
        users.cison.imports = [ ./domus/hm ];
      };
    }
  ];
}
