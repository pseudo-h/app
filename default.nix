with import ./definitions.nix;

let
  server = pkgs.haskell.packages.ghc865.callCabal2nix "app" ./. {};
  client = ghcjs86.callCabal2nix "app" ./. {};
in
{
  inherit server client;
  app = pkgs.runCommand "app" { inherit client server; } ''
    mkdir -p $out/{bin,static}
    cp ${server}/bin/* $out/bin/
    ${pkgs.tree}/bin/tree -a ${client}
    cp ${client}/bin/client.jsexe/all.js $out/static/
  '';
}
