let pinned-nixpkgs = builtins.fetchGit
    {
        url = "https://github.com/nixos/nixpkgs";
        ref = "master";
        rev = "859516a1a5af797ab11ac7597aa77a670877d87f";
    };
in with import pinned-nixpkgs {};
runCommand "rdf-env"
{
    buildInputs =
        let thisghc = haskell.packages.ghc924.ghcWithPackages
            (p: [ p.cabal-install
                  p.ghcid
                ]);
        in [ thisghc
             binutils
           ];
} ""
