import System.IO
import Data.Char
import qualified Data.Set as Set
import Categories.Category
import Categories.FinSet

a = Set.fromList [0, 1, 2, 3]
f = constructFinSetArrow a (\x -> x*x) Set.empty

b = codomain f

g = constructFinSetArrow b (floor . sqrt . fromIntegral) (Set.fromList [0, 1])

h = constructFinSetArrow a (*3) (Set.fromList [5])
c = codomain h

tagO :: Set.Set Int -> String 
tagO x 
    | x == a = "a"
    | x == b = "b"
    | x == c = "c"
    | otherwise = "Untaged"

tagA :: FinSetArrow Int -> String 
tagA x 
    | compareFinSetArrow x f = "F"
    | compareFinSetArrow x g = "G"
    | compareFinSetArrow x h = "H"
    | otherwise = "Untaged"


printArrow :: Cat o a -> (o -> String) -> (a -> String) -> a -> String
printArrow cat tagO tagA a = 
    "\t" ++ (tagO.source cat) a ++
    " -> " ++
    (tagO.target cat) a ++
    "[label=\"" ++
    tagA a ++
    "\"];\n"

constructGraph :: Cat o a -> (o -> String) -> (a -> String) -> [a] -> String
constructGraph cat tagO tagA arrows =
    "digraph G {\n" ++
    concatMap (printArrow cat tagO tagA) arrows  ++
    "}\n"

example = constructGraph finIntSetCat tagO tagA [f, g, h]

main = do  
    writeFile "girlfriendcaps.txt" example