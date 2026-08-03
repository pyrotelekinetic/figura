{ ... }: {

# TODO: maybe keep .inputrc in the store and set $INPUTRC
# TODO: test

users.users.cison.maid = {
  file.home.".inputrc" = {
    source = ./inputrc;
  };
};

}
