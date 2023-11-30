{ ... }: {

services.syncthing = {
  settings = {
    devices.onyx.id =
      "EK4W4BH-7BFCSBI-OGVO46Y-VYF4NPS-FB35OZZ-DZ72XCA-Q42B5PG-5GY2KA2";
    folders.clonehero-songs = {
      path = "~/data/clonehero-songs";
      id = "rjbfx-zwfxf";
      devices = [ "onyx" ];
    };
  };
};


}
