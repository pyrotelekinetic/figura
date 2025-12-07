{ nixosConfig, pkgs, ... }: {

wrappers.vim = {
  basePackage = pkgs.vim-full.override {
    features = "normal";
    guiSupport = false;
    luaSupport = false;
    perlSupport = false;
    pythonSupport = false;
    rubySupport = false;
    nlsSupport = false;
    tclSupport = false;
    multibyteSupport = true;
    cscopeSupport = false;
    netbeansSupport = false;
    ximSupport = true;
    darwinSupport = pkgs.stdenv.isDarwin;
    ftNixSupport = true;
  };
  prependFlags = let
    inherit (nixosConfig.users.users.cison) home;
    vimDir = ./vimdir;
    stateDir = home + "/.local/state/vim";
    vimrc = pkgs.runCommand "vimrc" {
      inherit vimDir stateDir;
    } "substituteAll ${./vimrc} $out";
  in [ "-u" vimrc ];
};

}
