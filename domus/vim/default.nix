{ nixosConfig, pkgs, ... }: {

wrappers.vim = {
  basePackage = pkgs.vim_configurable.override {
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
  flags = let
    vimDir = ./vimdir;
    homeDir = nixosConfig.users.users.cison.home;
    vimrc = pkgs.runCommand "vimrc" {
      inherit vimDir homeDir;
    } "substituteAll ${./vimrc} $out";
  in [ "-u" vimrc ];
};

}
