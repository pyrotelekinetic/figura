{

description = "Home Manager and NixOS configurations of pyrotelekinetic";

inputs = {
  disko = {
    url = "github:nix-community/disko?ref=latest";
    inputs.nixpkgs.follows = "nixpkgs";
  };
  home-manager = {
    url = "github:nix-community/home-manager?ref=master";
    inputs.nixpkgs.follows = "nixpkgs";
  };
  lanzaboote = {
    url = "github:nix-community/lanzaboote?ref=v0.4.1";
    inputs.nixpkgs.follows = "nixpkgs";
  };
  lix-module = {
    url = "git+https://git.lix.systems/lix-project/nixos-module?ref=release-2.91";
    inputs.nixpkgs.follows = "nixpkgs";
  };
  nixos-hardware.url = "github:NixOS/nixos-hardware?ref=master";
  nixpkgs.url = "github:NixOS/nixpkgs?ref=nixos-unstable";
  pinputs = {
    url = "github:pyrotelekinetic/pinputs?ref=main";
    inputs.nixpkgs.follows = "nixpkgs";
  };
  pyroscheme.url = "github:pyrotelekinetic/pyroscheme?ref=main";
  pyrosite.url = "github:pyrotelekinetic/pyrotelekinetic.github.io?ref=main";
  sops-nix = {
    url = "github:Mic92/sops-nix?ref=master";
    inputs.nixpkgs.follows = "nixpkgs";
  };
  wrapper-manager = {
    url = "github:viperML/wrapper-manager?ref=master";
    inputs.nixpkgs.follows = "nixpkgs";
  };
};

outputs = inputs: let
  inherit (inputs) nixpkgs;
  inherit (nixpkgs.lib) flip genAttrs;
  hosts = flip genAttrs (import ./mkSystem.nix inputs);
  allSystems = output: genAttrs nixpkgs.lib.systems.flakeExposed
    (system: output nixpkgs.legacyPackages.${system});
in {
  nixosConfigurations = hosts [
    "sol"
    "vega"
    "luna"
    "alsafi"
  ];

  images.luna = let
    luna-img = inputs.self.nixosConfigurations.luna.extendModules {
      modules = [
        "${nixpkgs}/nixos/modules/installer/sd-card/sd-image-aarch64.nix"
        {
          nixpkgs.buildPlatform.system = "x86_64-linux";
          documentation.enable = false;
        }
      ];
    };
  in
    luna-img.config.system.build.sdImage;

  formatter = allSystems (pkgs:
    pkgs.writeShellApplication {
      name = "deadnix";
      runtimeInputs = [ pkgs.deadnix ];
      text = "deadnix -e";
    }
  );

  checks = allSystems (pkgs: {
    default = pkgs.runCommand "deadnix-check" {} ''
      ${pkgs.lib.getExe pkgs.deadnix} --fail ${inputs.self}
      touch $out
    '';
  });

};

}
