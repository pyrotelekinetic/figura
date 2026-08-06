{ ... }: {

# TODO: test; use `niri validate`

users.users.cison.maid = {
  file.xdg_config."niri/config.kdl" = {
    source = ./config.kdl;
  };
};

}
