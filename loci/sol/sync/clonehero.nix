{ ... }: {

services.syncthing = {
  settings = {
    devices.onyx.id =
      "UM5VJDH-JHTQ7M3-M6OGU72-IQUDI6W-HQNFJ6R-PMDYYJI-VOHKL4E-B76J4AM";
    folders.clonehero-songs = {
      path = "~/data/clonehero-songs";
      id = "rjbfx-zwfxf";
      devices = [ "onyx" ];
    };
  };
};


}
