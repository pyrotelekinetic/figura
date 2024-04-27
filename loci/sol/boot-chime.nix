{ pkgs, lib, config, ... }: {

systemd.services.boot-chime = {
  wantedBy = [ "multi-user.target" ];
  after = [ "multi-user.target" ];
  serviceConfig = {
    Type = "oneshot";
    RemainAfterExit = false;
  };
  script = let
    aplay = lib.getExe' pkgs.alsa-utils "aplay";
    ffmpeg = lib.getExe pkgs.ffmpeg;
    riffs = pkgs.fetchFromGitHub {
      owner = "pyrotelekinetic";
      repo = "seinfeld-riffs";
      rev = "e38fc089b68fbca5796ac4bdb55f34a5e005b1fd";
      hash = "sha256-FkF5UmKsSkbjrPqYAt7xhOn4aiUMBxotl3JXzweSaNI=";
    };
  in ''
    RIFF=$(shuf -n 1 -e ${riffs}/riffs/*)
    ${ffmpeg} -i "$RIFF" -c pcm_s32le -f wav - | \
    ${aplay} --device=hdmi:CARD=HDMI,DEV=3 -r44100 -f S32_LE -
  '';
};

}
