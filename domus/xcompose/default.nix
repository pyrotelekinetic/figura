{ config, lib, ... }: {

# TODO: maybe keep .XCompose in the store and set $XCOMPOSEFILE
# TODO: test
# TODO: build .XCompose in nix with a cool module

users.users.cison.maid = lib.mkIf config.head.graphical {
  file.home.".XCompose" = {
    source = ./XCompose;
  };
};

}
