{ lib, ... }: {

imports = [
  ./hardware.nix
];

# Don't want sshd on a portable machine that connects to lots of different networks
services.openssh.enable = lib.mkForce false;

}
