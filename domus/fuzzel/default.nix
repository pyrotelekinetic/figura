{ pkgs, inputs, ... }: {

wrappers.fuzzel = {
  basePackage = pkgs.fuzzel;
  prependFlags = let
    config = pkgs.runCommand "fuzzel.ini"
    inputs.pyroscheme.lib.colors
    "substituteAll ${./fuzzel.ini} $out";
  in [ "--config" config ];
    postBuild = ''
      echo "checking config"
      # Set locale for unicode support
      LC_ALL=C.utf8 $out/bin/fuzzel --check-config
    '';
};

}
