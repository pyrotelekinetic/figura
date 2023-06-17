{

description = "Home Manager and NixOS configurations of pyrotelekinetic";

inputs = {
  nixpkgs = {
    type = "github";
    owner = "NixOS";
    repo = "nixpkgs";
    ref = "nixos-22.11";
  };
  home-manager = {
    type = "github";
    owner = "nix-community";
    repo = "home-manager";
    ref = "release-22.11";
    inputs.nixpkgs.follows = "nixpkgs";
  };
  pyroscheme = {
    type = "github";
    owner = "pyrotelekinetic";
    repo = "pyroscheme";
    ref = "main";
  };
};

outputs = { self, nixpkgs, home-manager, pyroscheme }: {
  nixosConfigurations.sol = nixpkgs.lib.nixosSystem {
    system = "x86_64-linux";
    modules = [
      ./mundus

      {
        nix.registry.nixpkgs.flake = nixpkgs;
        environment.etc."nix/inputs/nixpkgs".source = nixpkgs.outPath;
        nix.nixPath = [ "nixpkgs=/etc/nix/inputs/nixpkgs" ];
      }

      home-manager.nixosModules.home-manager {
        home-manager = {
          extraSpecialArgs = { colors = pyroscheme.colors; };
          useGlobalPkgs = true;
          useUserPackages = true;
          users.cison = {
            imports = [ ./domus ];
            graphical = {
              enable = true;
              games = true;
            };
          };
        };
      }
    ];
  };
};

}
