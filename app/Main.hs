import System.IO
import Data.Char
import Categories.Category
import Categories.Finite
import Categories.Graph

nodeA = Node "a" (show A)
nodeB = Node "b" (show B)
nodeBA = Node "ba" (show B)
nodeC = Node "c" (show C)

edgeF = Edge (show F) nodeA nodeBA
edgeJ = Edge (show G) nodeB
edgeG = Edge (show H) nodeB nodeC
edgeH = Edge (show K) nodeA nodeC

example = constructGraph [nodeA, nodeB, nodeBA, nodeC] [edgeF, edgeG, edgeH]

main = do  
    writeFile "girlfriendcaps.txt" example