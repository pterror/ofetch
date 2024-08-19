{ lib
, rustPlatform
, pkg-config
, udev
, libinput
}:
ocamlPackages.buildDunePackage rec {
  pname = "ofetch";
  version = "0.0.1";
}
no