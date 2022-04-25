build:
	cabal build

run:
	make
	cabal exec category-theory | dot -Tsvg -Kcirco  > graph.svg