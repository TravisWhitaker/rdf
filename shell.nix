let pinned-nixpkgs = builtins.fetchGit
    {
        url = "https://github.com/nixos/nixpkgs";
        ref = "master";
        rev = "bfbfb344598544058481e254b9f0f174d05b5e6a";
    };
in with import pinned-nixpkgs {};
runCommand "rdf-env"
{
    buildInputs =
        let thisghc = haskell.packages.ghc963.ghcWithPackages
            (p: [ p.cabal-install
                ]);
        in [ thisghc
             binutils
           ];
} ""
