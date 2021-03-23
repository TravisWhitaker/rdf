let pinned-nixpkgs = builtins.fetchGit
    {
        url = "https://github.com/nixos/nixpkgs";
        ref = "master";
        rev = "80cef4476a6d3c2a7cb5ad46c4490eed7ac39a28";
    };
in with import pinned-nixpkgs {};
runCommand "rdf-env"
{
    buildInputs =
        let thisghc = haskell.packages.ghc8104.ghcWithPackages
            (p: [ p.cabal-install
                  p.ghcid
                ]);
        in [ thisghc
             binutils
           ];
} ""
