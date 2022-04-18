import System.IO
import Data.Char
import Categories.Category
import Categories.Graph

a = Node "a" "A"
b = Node "b" "B"
ba = Node "ba" "B"
c = Node "c" "C"

f = Edge "f" a ba
j = Edge "j" a b
g = Edge "g" b c
h = Edge "h" a c
k = Edge "k" a c

defineNode :: Node -> String
defineNode n =
    "\t" ++ name n ++
    "[label=\"" ++
    labelNode n ++
    "\"];\n"

printEdge :: Edge -> String
printEdge e = 
    "\t" ++ (name.source graphCat) e ++
    " -> " ++
    (name.target graphCat) e ++
    "[label=\"" ++
    labelEdge e ++
    "\"];\n"

constructGraph :: [Node] -> [Edge] -> String
constructGraph nodes edges =
    "digraph G {\n" ++
    concatMap defineNode nodes  ++
    concatMap printEdge edges  ++
    "}\n"

example = constructGraph [a, b, ba, c] [f, g, h, k, j]

main = do  
    writeFile "girlfriendcaps.txt" example