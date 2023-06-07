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
};

outputs = { nixpkgs, home-manager, ... }: {
  nixosConfigurations.sol = nixpkgs.lib.nixosSystem {
    system = "x86_64-linux";
    modules = [
      ./figura/configuration.nix
      ./figura/hardware-configuration.nix
      ./figura/pyrosite.nix
      ./figura/swap.nix

      {
        nix.registry.nixpkgs.flake = nixpkgs;
        environment.etc."nix/inputs/nixpkgs".source = nixpkgs.outPath;
        nix.nixPath = [ "nixpkgs=/etc/nix/inputs/nixpkgs" ];
      }
    ];
  };

  homeConfigurations.cison = home-manager.lib.homeManagerConfiguration {
    pkgs = import nixpkgs {
      config.allowUnfree = true;
      system = "x86_64-linux";
    };
    modules = [
      ./domus/home.nix
      ./domus/sway.nix
      ./domus/vim.nix
      ./domus/games.nix

      (args: {
        nix.registry.nixpkgs.flake = nixpkgs;
        xdg.configFile."nix/inputs/nixpkgs".source = nixpkgs.outPath;
        home.sessionVariables.NIX_PATH = "nixpkgs=${args.config.xdg.configHome}/nix/inputs/nixpkgs$\{NIX_PATH:+:$NIX_PATH}";
      })
    ];
  };
};

}
