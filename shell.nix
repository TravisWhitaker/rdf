let pinned-nixpkgs = builtins.fetchGit
    {
        url = "https://github.com/nixos/nixpkgs";
        ref = "master";
        rev = "66b571c10b0fdac6739fd51d04e87f8bce77f91e";
    };
in with import pinned-nixpkgs {};
runCommand "rdf-env"
{
    buildInputs =
        let thisghc = haskell.packages.ghc865.ghcWithPackages
            (p: [ p.cabal-install
                  p.ghcid
                ]);
        in [ thisghc
             binutils
           ];
} ""
