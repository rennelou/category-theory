build:
	cabal build

run:
	cabal exec category-theory | dot -Tsvg -Kcirco  > graph.svg