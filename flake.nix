{
  description = "aoc";

  inputs.nixpkgs.url = "github:NixOS/nixpkgs/release-23.11";
  inputs.flake-utils.url = "github:numtide/flake-utils";

  outputs = inputs:
    let
      overlay = final: prev: {
        haskell = prev.haskell // {
          packageOverrides = hfinal: hprev:
            prev.haskell.packageOverrides hfinal hprev // {
              aoc = hfinal.callCabal2nix "aoc" ./. { };
            };
        };
        aoc = final.haskell.lib.compose.justStaticExecutables final.haskellPackages.aoc;
      };
      perSystem = system:
        let
          pkgs = import inputs.nixpkgs { inherit system; overlays = [ overlay ]; };
          hspkgs = pkgs.haskellPackages;
        in
        {
          devShells = rec {
            default = aoc-shell;
            aoc-shell = hspkgs.shellFor {
              withHoogle = true;
              packages = p: [ p.aoc ];
              buildInputs = [
                hspkgs.cabal-install
                hspkgs.haskell-language-server
                hspkgs.hlint
                hspkgs.ormolu
                pkgs.bashInteractive
              ];
            };
          };
          packages = rec {
            default = aoc;
            aoc = pkgs.aoc;
          };
        };
    in
    { inherit overlay; } // inputs.flake-utils.lib.eachDefaultSystem perSystem;
}
