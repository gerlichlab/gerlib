{
  pkgs ? import (builtins.fetchGit {
    url = "https://github.com/NixOS/nixpkgs/";
    ref = "refs/tags/24.05";
  }) {}, 
  dev ? true,
  javaSpec ? "jdk21",
}:
let 
   sysPkgs = [ pkgs.${javaSpec} ];
   scalaDevTools = with pkgs; [ ammonite coursier sbt-with-scala-native ];
in
pkgs.mkShell {
  name = "gerlib-env";
  buildInputs = sysPkgs ++ (
    if dev then [ scalaDevTools ] else []
  );
  shellHook = ''
    export LD_LIBRARY_PATH="${pkgs.zlib}/lib:${pkgs.stdenv.cc.cc.lib}/lib"
  '';
}
