{ local ? (import <local> {}) }:

local.lib.allCall (import ./package.nix) {igraph = local.pkgs.igraph;}
