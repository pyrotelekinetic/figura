{ lib
, stdenv
, fetchFromGitHub
, kernel
}:

stdenv.mkDerivation (finalAttrs: {
  pname = "hid-wiimote-plus";
  version = "0.9.2";
  src = fetchFromGitHub {
    owner = "dkosmari";
    repo = "hid-wiimote-plus";
    rev = "refs/tags/v${finalAttrs.version}";
    hash = "sha256-kevNDAbqSazCvzp7WYpyo+dyxtX9ekz7OFUC00MYd+Y=";
  };

  outputs = [ "out" "udev" ];

  nativeBuildInputs = kernel.moduleBuildDependencies;
  hardeningDisable = [ "pic" ];

  makeFlags = kernel.makeFlags ++ [
    "KDIR=${kernel.dev}/lib/modules/${kernel.modDirVersion}/build"
  ];

  installPhase = ''
    install -Dm 644 99-wiimote.rules $udev/etc/udev/rules.d/99-wiimote.rules
    install -Dm 644 hid-wiimote.ko $out/lib/modules/${kernel.modDirVersion}/updates/hid-wiimote.ko
  '';

  meta = {
    description = "Improved Linux input driver for the Wiimote devices";
    homepage = "https://github.com/dkosmari/hid-wiimote-plus";
    license = lib.licenses.gpl2Plus;
    platforms = lib.platforms.linux;
  };

})
