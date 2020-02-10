with (import (builtins.fetchTarball {
  url = "https://github.com/dmjio/miso/archive/561ffad.tar.gz";
  sha256 = "1wwzckz2qxb873wdkwqmx9gmh0wshcdxi7gjwkba0q51jnkfdi41";
}) {});

let
  servant-src = pkgs.fetchFromGitHub { 
    owner = "haskell-servant";
    repo = "servant";
    rev = "5998429" ;
    sha256 = "1p21x6df0j6zsmjf4z9cahbi15mar80na8rx8hl9mkb9pfrj9dic";
  };
in

{
  inherit pkgs;

  ghcjs86 = pkgs.haskell.packages.ghcjs86.override {
    overrides = self: super: with pkgs.haskell.lib; {
      servant-client-ghcjs = pkgs.haskell.packages.ghcjs.callCabal2nix "${servant-src}/servant-client-ghcjs" {};
    };
  };
}
