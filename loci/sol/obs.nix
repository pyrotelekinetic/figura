{ pkgs, ... }: {

environment.systemPackages = with pkgs; [
  (wrapOBS {
    plugins =  with obs-studio-plugins; [
      obs-pipewire-audio-capture
      obs-vaapi
    ];
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
