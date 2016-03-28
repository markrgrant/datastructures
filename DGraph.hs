module DGraph (
    DGraph,
    create,
    fromList,
    toList,
    addEdge,
    adj,
    numVertices,
    numEdges,
    reverse
) where


import Prelude hiding (reverse)
import Data.List (foldl')
import qualified Data.Vector as V


-- Adjacency list representation
newtype DGraph = DGraph AdjList deriving (Show)


type AdjList = V.Vector [Int]


-- Create a graph with n vertices and no edges.
create :: Int -> DGraph
create n = DGraph $ V.replicate n []


-- Create a new graph of the given size from an edge list where n
-- is the number of vertices and edges is a list of edges between the
-- vertices.
fromList :: Int -> [(Int, Int)] -> DGraph 
fromList n = foldl' (\g (x,y) -> addEdge g x y) (create n)


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

