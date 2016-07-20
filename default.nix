{ local ? (import ../../../nixlocal {}) }:

local.lib.allCall (import ./package.nix) {igraph = local.pkgs.igraph; ocamlfind = local.pkgs.findlib;}
