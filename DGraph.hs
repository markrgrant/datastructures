module DGraph (
    DGraph,
    create,
    random,
    fromList,
    fromFile,
    toList,
    addEdge,
    adj,
    numVertices,
    numEdges,
    reverse
) where


import Prelude hiding (reverse)
import Data.List (foldl', groupBy, sort)
import qualified Data.Vector as V
import System.Random (randomRs, StdGen)


-- Adjacency list representation
newtype DGraph = DGraph AdjList deriving (Show)


-- A local type synonym for brevity
type AdjList = V.Vector [Int]


-- Create a graph with n vertices and no edges.
create :: Int -> DGraph
create n = DGraph $ V.replicate n []


-- Create a random graph with the given number of vertices and edges, where
-- the edges are chosen at random.
random :: Int -> Int -> StdGen -> DGraph
random v e g = fromList v $ take e $ pairs (randomRs (0,v-1) g)
  where pairs :: [a] -> [(a, a)]
        pairs (x:y:zs) = (x,y):pairs zs


-- Create a new graph of the given size from an edge list where n
-- is the number of vertices and edges is a list of edges between the
-- vertices.  To do this efficiently, accumulate the list of pairs
-- where the first element in the pair is the source vertex and the 
-- second element is a list of target vertices.  Then update the 
-- vector of the DGraph.
fromList :: Int -> [(Int, Int)] -> DGraph
fromList n = DGraph . (V.replicate n [] V.//) . groupEdges . sort
  where groupEdges :: [(Int, Int)] -> [(Int, [Int])]
        groupEdges = foldl' accEdges []
          where accEdges [] (fr,to) = [(fr, [to])]
                accEdges ((fr1, ts):xs) (fr2, to)
                  | fr1 == fr2 = (fr1, to:ts):xs
                  | otherwise = (fr2, [to]):(fr1, ts):xs


-- Return the adjacency list representation of the directed graph
-- as a list of tuples where the left hand side of the tuple is the source
-- vertex and the right hand side is the destination
toList :: DGraph -> [(Int, Int)]
toList (DGraph v) =  toList' v (V.length v - 1) []


toList' v (-1) xs = xs
toList' v i    xs = toList' v (i-1) xs' 
    where edges = v V.! i
          xs'   = foldl' (\acc to -> (to,i):acc) xs edges


-- Add an edge from vertex i to vertex j
addEdge :: DGraph -> Int -> Int -> DGraph
addEdge (DGraph v) i j = DGraph $ v V.// [(i, j:iedges)]
    where iedges = v V.! i


-- Get the vertices connected by outgoing edges from the vertex.
adj :: DGraph -> Int -> [Int]
adj (DGraph v) i = v V.! i


-- The total number of vertices in the graph.
numVertices :: DGraph -> Int
numVertices (DGraph v) = V.length v


-- The total number of edges in the graph.
numEdges :: DGraph -> Int
numEdges (DGraph v) = V.foldl' (\acc lst -> acc + length lst) 0 v


-- Reverse the edges in the graph
reverse :: DGraph -> DGraph
reverse (DGraph v) = fromList numVertices $ reverseEdges v (numVertices-1) []
    where numVertices = V.length v


reverseEdges :: AdjList -> Int -> [(Int, Int)] -> [(Int, Int)]
reverseEdges v (-1)  lst = lst
reverseEdges v fr    lst = reverseEdges v (fr-1) lst'
    where edges = v V.! fr
          lst'  = foldl' (\acc to -> (to,fr):acc) lst edges


-- Read a file containing a directed graph.  The first line in the file
-- contains the number of vertices.  the second line contains the number of
-- edges.  The remaining lines contain two numbers separated by a space,
-- the first number being the source vertex and the second number being
-- the destination vertex of a pair.  For example the following would
-- be the contents of a file specifying a directed graph containing 6
-- vertices and two edges with an edge from vertex 0 to vertex 5 and an edge
-- from vertex 3 to vertex 4.
-- 6
-- 2
-- 0 5
-- 3 4
fromFile :: FilePath -> IO DGraph
fromFile fp =  fmap fromString (readFile fp)


fromString :: String -> DGraph
fromString str = fromList numVertices edges
  where ls = lines str
        numVertices = read (head ls) :: Int
        numEdges    = read (head (tail ls)) :: Int
        edges = map (listToTuple . map read . words) (tail (tail ls))
        listToTuple :: [Int] -> (Int, Int)
        listToTuple [x,y] = (x,y)
