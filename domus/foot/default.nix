{ pkgs, lib, inputs, ... }: {

wrappers.foot = {
  basePackage = pkgs.foot;
  prependFlags = let
    config = pkgs.runCommand "foot.ini"
      inputs.pyroscheme.lib.colors
      "substituteAll ${./foot.ini} $out";
  in [ "--config" config ];
  postBuild = "$out/bin/foot --check-config";
};

}
