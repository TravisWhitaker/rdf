{ tryLatest ? false
}:
let pkgs = if tryLatest
           then {url = "https://github.com/nixos/nixpkgs"; ref = "master";}
           else import ./pinned.nix;
    pinned-nixpkgs = builtins.fetchGit pkgs;
in with import pinned-nixpkgs {};
runCommand "rdf-env"
{
    buildInputs =
        let thisghc = haskell.packages.ghc983.ghcWithPackages
            (p: []);
        in [ thisghc
             binutils
             cabal-install
           ];
} ""
