{ ... }: {

services.syncthing = {
  settings = {
    devices.vault.id =
      "3PC5XF2-ELUCF4L-F4YXZWD-KHUFWTJ-KLRK35B-FQMYYO3-A6UN4JR-NQPPTAY";
    folders.Factorio-Saves = {
      path = "~/.factorio/saves/sync";
      id = "xqg4g-nykzf";
      devices = [ "vault" ];
    };
  };
};

}
