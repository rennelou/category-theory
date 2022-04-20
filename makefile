build:
	cabal build

run:
	cabal exec category-theory | dot -Tsvg > graph.svg