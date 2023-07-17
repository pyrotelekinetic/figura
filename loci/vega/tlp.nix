{ ... }: {

services.tlp = {
  enable = true;
  settings = {
    TLP_DEFAULT_MODE = "BAT";
    START_CHARGE_THRESH_BAT0 = 75;
    STOP_CHARGE_THRESH_BAT0 = 90;
    RADEON_DPM_STATE_ON_AC = "performance";
    RADEON_DPM_STATE_ON_BAT = "battery";
    CPU_SCALING_GOVERNOR_ON_AC = "performance";
    CPU_SCALING_GOVERNOR_ON_BAT = "schedutil";
    CPU_BOOST_ON_AC = 1;
    CPU_BOOST_ON_BAT = 0;
    DEVICES_TO_DISABLE_ON_STARTUP = "bluetooth";
    DEVICES_TO_DISABLE_ON_BAT_NOT_IN_USE = "bluetooth";
    DEVICES_TO_DISABLE_ON_LAN_CONNECT = "wifi";
    DEVICES_TO_ENABLE_ON_LAN_DISCONNECT = "wifi";
    RUNTINE_PM_DRIVER_DENYLIST = "nvme amdgpu";
    DISK_DEVICES = "nvme0n1";
  };
};

}
