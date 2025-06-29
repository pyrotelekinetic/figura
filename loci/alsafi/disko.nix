{

disko.devices.disk.main = {
  device="/dev/sda";
  type = "disk";
  content = {
    type = "gpt";
    partitions = {
      ESP = {
        type = "EF00";
        size = "500M";
        content = {
          type = "filesystem";
          format = "vfat";
          mountpoint = "/efi";
          mountOptions = [ "umask=0077" "fmask=0022" "dmask=0022" ];
        };
      };
      nixos = {
        size = "100%";
        content = {
          type = "filesystem";
          format = "ext4";
          mountpoint = "/";
        };
      };
      swap = {
        size = "8G";
        content.type = "swap";
      };
    };
  };
};

}
