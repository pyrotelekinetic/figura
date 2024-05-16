{ pkgs, ... }: {

systemd.services.boot-chime = {
  wantedBy = [ "multi-user.target" ];
  after = [ "multi-user.target" ];
  serviceConfig = {
    Type = "oneshot";
    RemainAfterExit = false;
  };

  environment.RIFFS = pkgs.fetchFromGitHub {
    owner = "pyrotelekinetic";
    repo = "seinfeld-riffs";
    rev = "40e0e69bbb2fbaae5829873fea7bb2d8f3bb4531";
    hash = "sha256-7PxTcy/jvRoIT+8eyG2oMaYI2Unz+P06laWKiJqPUnU=";
  };

  path = [
    pkgs.alsa-utils
    pkgs.ffmpeg
  ];

  script = ''
    RIFF=$(shuf -n 1 -e $RIFFS/riffs/short/*)
    ffmpeg -i "$RIFF" -c pcm_s32le -f wav - | \
    aplay --device=hdmi:CARD=HDMI,DEV=3 -r44100 -f S32_LE -
  '';
};

}
