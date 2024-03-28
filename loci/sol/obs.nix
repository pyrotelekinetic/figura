{ pkgs, ... }: {

environment.systemPackages = with pkgs; [
  (wrapOBS {
    plugins =  [ obs-studio-plugins.obs-pipewire-audio-capture ];
  })
  losslesscut-bin
  ffmpeg
];

hardware.opengl.extraPackages = with pkgs; [
  amdvlk
  vaapiVdpau
  libvdpau-va-gl
];

}
