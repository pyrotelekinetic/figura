{

description = "Home Manager and NixOS configurations of pyrotelekinetic";

inputs = {
  nixpkgs = {
    type = "github";
    owner = "NixOS";
    repo = "nixpkgs";
    ref = "nixos-unstable";
  };
  nixos-hardware = {
    type = "github";
    owner = "NixOS";
    repo = "nixos-hardware";
    ref = "master";
  };
  home-manager = {
    type = "github";
    owner = "nix-community";
    repo = "home-manager";
    ref = "master";
    inputs.nixpkgs.follows = "nixpkgs";
  };
  sops-nix = {
    type = "github";
    owner = "Mic92";
    repo = "sops-nix";
    inputs = {
      nixpkgs.follows = "nixpkgs";
      nixpkgs-stable.follows = "nixpkgs";
    };
  };
  pyroscheme = {
    type = "github";
    owner = "pyrotelekinetic";
    repo = "pyroscheme";
    ref = "main";
  };
};

outputs = inputs@{
  self,
  nixpkgs,
  nixos-hardware,
  home-manager,
  sops-nix,
  pyroscheme
}: let
  mkSystem = import ./mkSystem.nix inputs;
in {
  nixosConfigurations = (
    mkSystem {
      host = "sol";
      system = "x86_64-linux";
    }
  ) // (
    mkSystem {
      host = "vega";
      system = "x86_64-linux";
    }
  );
};

}
