{stdenv, bap, findlib, bin_prot, igraph, clang, ghcWithPackages}:

let ghc = ghcWithPackages (pkgs: [pkgs.multiset]); in

stdenv.mkDerivation rec {
  name = "bcc";
  version = "0";
  src = ./.;

  buildInputs = [ clang bin_prot bap findlib igraph ghc ];

  buildPhase = ''
    clang -ligraph cluster/cluster.c -o cluster-callgraph
    ${bap}/bin/bapbuild -pkg bin_prot.syntax plugin/bcc_dump.plugin plugin/strings.plugin plugin/dump_callgraph.plugin
    ghc --make aggregate/Test aggregate/Util.hs aggregate/Parse.hs
    ghc --make aggregate/Convert aggregate/Util.hs aggregate/Parse.hs
    ghc --make aggregate/Merge aggregate/Util.hs aggregate/Parse.hs
    sed -i scripts/generate_db -e s#PUT_PLUGIN_PATH_HERE#$out/share/bap/plugins#g
    sed -i scripts/test_program -e s#PUT_PLUGIN_PATH_HERE#$out/share/bap/plugins#g
  '';

  installPhase = ''
    install -D cluster-callgraph $out/bin/cluster-callgraph
    install -D bcc_dump.plugin $out/share/bap/plugins/bcc_dump.plugin
    install -D strings.plugin $out/share/bap/plugins/strings.plugin
    install -D dump_callgraph.plugin $out/share/bap/plugins/dump_callgraph.plugin
    install -D aggregate/Test $out/bin/test-abberations
    install -D aggregate/Merge $out/bin/merge-db
    install -D aggregate/Convert $out/bin/load-constants
    install -D scripts/test_program $out/bin/test_program
    install -D scripts/generate_db $out/bin/generate_db
  '';

  meta = with stdenv.lib; {
    description = "Binary Constant Clustering";
    license = licenses.mit;
  };
}
