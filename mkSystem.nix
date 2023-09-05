inputs: { host, system }: with inputs; {

${host} = nixpkgs.lib.nixosSystem {
  system = system;
  specialArgs = { inherit inputs; };
  modules = [
    ./mundus
    "${self}/loci/${host}"
    sops-nix.nixosModules.sops
    pinputs.nixosModules.default
    {
      system.configurationRevision = self.rev or "DIRTY";
      networking.hostName = host;
      pins = { inherit inputs; };
    }
    home-manager.nixosModules.home-manager {
      home-manager = {
        extraSpecialArgs = { colors = pyroscheme.colors; };
        useGlobalPkgs = true;
        useUserPackages = true;
        users.cison.imports = [ ./domus ];
      };
    }
  ];
};

}
