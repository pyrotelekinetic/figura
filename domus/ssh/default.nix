{ ... }: {

users.users.cison.maid = {
  file.home = {
    ".ssh/config".text = ''
      Host *
      AddKeysToAgent no
      Compression no
      ControlMaster auto
      ControlPath %d/.ssh/control/%r@%n:%p
      ControlPersist 5m
      ForwardAgent no
    '';

    # TODO: open a pr on nix-maid to create dirs
    ".ssh/control/.mkdir".source = "/dev/null";

    ".ssh/allowed_signers".source = ./allowed_signers;
  };
};

}
