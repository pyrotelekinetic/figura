{ ... }: {

imports = [
  ./hardware.nix
  ../share/pyrosite.nix
];

networking = {
  hosts = {
    "192.168.1.2" = [ "luna" ];
  };
  interfaces.enp3s0.wakeOnLan.enable = true;
};

}
