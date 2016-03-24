-- An undirected multigraph implementation.  Self edges and parallel edges
-- are allowed.
module Graph (
    Graph,
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
data Graph = Graph (V.Vector [Int]) deriving (Show)


-- Create a graph with n vertices and no edges.
create :: Int -> Graph
create n = Graph $ V.replicate n []


-- Create a new graph of the given size from an edge list where n
-- is the number of vertices and edges is a list of edges between the
-- vertices.
fromList :: Int -> [(Int, Int)] -> Graph 
fromList n = foldl' (\g (x,y) -> addEdge g x y) (create n)


-- Add an edge between vertices i and j.
addEdge :: Graph -> Int -> Int -> Graph
addEdge (Graph v) i j = Graph v''
    where iedges = v V.! i
          v' = v V.// [(i, j:iedges)]
          jedges = v' V.! j
          v'' = v' V.// [(j, i:jedges)]


-- Get the vertices adjacent to vertex i.
adj :: Graph -> Int -> [Int]
adj (Graph v) i = v V.! i


-- The total number of vertices in the graph.
numVertices :: Graph -> Int
numVertices (Graph v) = V.length v


-- The total number of edges in the graph.
numEdges :: Graph -> Int
numEdges (Graph v) = doubleCount `div` 2
    where doubleCount = V.foldl' (\acc lst -> acc + length lst) 0 v 
