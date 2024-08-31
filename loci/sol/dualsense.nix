{ pkgs, ... }: {

home-manager.users.cison.wayland.windowManager.sway.config.input = {
  # Stop PS5 controller from being a touchpad
  "1356:3302:Sony_Interactive_Entertainment_DualSense_Wireless_Controller_Touchpad".events = "disabled"; # USB
  "1356:3302:DualSense_Wireless_Controller_Touchpad".events = "disabled"; # Bluetooth
};

# set trigger haptics, check battery, power off, etc
environment.systemPackages = [ pkgs.dualsensectl ];

}
