with import <nixpkgs> {};

stdenv.mkDerivation {
  name = "xmonad";
  buildInputs = [stack gnumake];
}
