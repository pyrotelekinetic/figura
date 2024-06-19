{ pkgs, config, ... }: {

environment.systemPackages = [ pkgs.oci-cli pkgs.openssl ];

sops.secrets = {
  "oci-script/CompIdFile".sopsFile = ../secrets.yaml;
  "oci-script/SubnetIdFile".sopsFile = ../secrets.yaml;
  "oci-script/ImageIdFile".sopsFile = ../secrets.yaml;
};

systemd = {
  timers."oci-script" = {
    wantedBy = [ "timers.target" ];
    timerConfig = {
      OnBootSec = "5m";
      OnUnitActiveSec = "5m";
    };
  };
  services."oci-script" = {
    environment = {
      AD1 = "oJmo:PHX-AD-1";
      AD2 = "oJmo:PHX-AD-2";
      AD3 = "oJmo:PHX-AD-3";
      CompIdFile = config.sops.secrets."oci-script/CompIdFile".path;
      SubnetIdFile = config.sops.secrets."oci-script/SubnetIdFile".path;
      ImageIdFile = config.sops.secrets."oci-script/ImageIdFile".path;
    };
    script = ''
      CompId=$(<$CompIdFile)
      SubnetId=$(<$SubnetIdFile)
      ImageId=$(<$ImageIdFile)
      for AD in $AD1 $AD2 $AD3; do \
      ${pkgs.oci-cli}/bin/oci compute instance launch \
        --availability-domain $AD \
        --compartment-id $CompId \
        --shape VM.Standard.A1.Flex \
        --subnet-id $SubnetId \
        --assign-private-dns-record true \
        --assign-public-ip false \
        --availability-config file://${./availabilityConfig.json} \
        --display-name new-instance \
        --image-id $ImageId \
        --instance-options file://${./instanceOptions.json} \
        --shape-config file://${./shapeConfig.json} \
        --ssh-authorized-keys-file ${./authorized_keys} \
      || true; done
    '';
    serviceConfig = {
      Type = "oneshot";
    };
  };
};

}
