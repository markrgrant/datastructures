module DGraph (
    DGraph,
    create,
    fromList,
    addEdge,
    adj,
    numVertices,
    numEdges
) where


import Data.List (foldl')
import qualified Data.Vector as V


-- Adjacency list representation
data DGraph = DGraph (V.Vector [Int]) deriving (Show)


-- Create a graph with n vertices and no edges.
create :: Int -> DGraph
create n = DGraph $ V.replicate n []


-- Create a new graph of the given size from an edge list where n
-- is the number of vertices and edges is a list of edges between the
-- vertices.
fromList :: Int -> [(Int, Int)] -> DGraph 
fromList n = foldl' (\g (x,y) -> addEdge g x y) (create n)


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
