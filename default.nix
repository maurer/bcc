{ local ? (import ~/.nix-defexpr/local {}) }:

local.lib.allCall (import ./package.nix) {igraph = local.pkgs.igraph;}
