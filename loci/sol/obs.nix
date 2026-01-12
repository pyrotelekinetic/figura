{ pkgs, ... }: {

programs.obs-studio = {
  enable = true;
  enableVirtualCamera = true;
  plugins = [
    pkgs.obs-studio-plugins.obs-pipewire-audio-capture
    pkgs.obs-studio-plugins.obs-vaapi
    pkgs.obs-studio-plugins.wlrobs
  ];
};

environment.systemPackages = with pkgs; [
  losslesscut-bin
  ffmpeg
];

hardware.graphics.extraPackages = with pkgs; [
  libva-vdpau-driver
  libvdpau-va-gl
];

}
