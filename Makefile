BB=bapbuild
PKGS=ocamlgraph,sqlite3EZ

all: callgraph.ml extractor.ml callstringtree.ml
	$(BB) -pkgs $(PKGS) callgraph.plugin
	$(BB) -pkgs $(PKGS) extractor.plugin
	$(BB) -pkgs $(PKGS) callstringtree.plugin
