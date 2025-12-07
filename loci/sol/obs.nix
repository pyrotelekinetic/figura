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

hardware.graphics.extraPackages = with pkgs; [
  libva-vdpau-driver
  libvdpau-va-gl
];

}
