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
  pinputs = {
    type = "github";
    owner = "pyrotelekinetic";
    repo = "pinputs";
    ref = "main";
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
  pinputs,
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
  ) // (
    mkSystem {
      host = "luna";
      system  = "aarch64-linux";
    }
  );

  images.luna = let
    luna-img = self.nixosConfigurations.luna.extendModules {
      modules = [
        "${nixpkgs}/nixos/modules/installer/sd-card/sd-image-aarch64.nix"
        {
          nixpkgs.buildPlatform.system = "x86_64-linux";
          users.users.cison.initialHashedPassword = "";
          documentation.enable = false;
        }
      ];
    };
  in
    luna-img.config.system.build.sdImage;

};

}
