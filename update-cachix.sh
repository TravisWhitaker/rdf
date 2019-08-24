#! /usr/bin/env nix-shell
#! nix-shell -i bash -p parallel

nix-shell --run "echo built shell"
# nix-store -qR --include-outputs $(nix-instantiate shell.nix) | parallel -j0 -I% 'echo % | cachix push haskell-bootcamp'
nix-store -qR --include-outputs $(nix-instantiate shell.nix) | cachix push rdf
