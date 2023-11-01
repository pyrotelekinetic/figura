{ pkgs, ... }: {

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
    vimrc = pkgs.writeText "vimrc" (import ./vimrc.nix);
  in [ "-u" vimrc ];
};

}
