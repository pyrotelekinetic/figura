{ config, pkgs, inputs, lib, ... }: {

wrappers.foot = lib.mkIf config.head.graphical {
  basePackage = pkgs.foot;
  prependFlags = let
    config = pkgs.runCommand "foot.ini"
      inputs.pyroscheme.lib.colors
      "substituteAll ${./foot.ini} $out";
  in [ "--config" config ];
  postBuild = "$out/bin/foot --check-config";
};

}
