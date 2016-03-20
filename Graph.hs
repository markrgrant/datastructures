-- an undirected multigraph implementation
module Graph (
    Graph,
    create,
    fromList,
    addEdge,  -- insert an edge between the given vertices
    adj,
    numVertices,
    numEdges
) where

import Data.List (foldl')
import qualified Data.Vector as V


-- adjacency list represetation
data Graph = Graph (V.Vector [Int]) deriving (Show)


-- create a graph with n vertices and no edges
create :: Int -> Graph
create n = Graph $ V.replicate n []


-- create a new graph of the given size from an edge list where n
-- is the number of vertices and edges is a list of edges between the
-- vertices
fromList :: Int -> [(Int, Int)] -> Graph 
fromList n edges = 
    foldl' (\g (x,y) -> addEdge g x y) (create n) edges


-- Add an edge between vertices i and j
addEdge :: Graph -> Int -> Int -> Graph
addEdge (Graph v) i j = Graph $ v V.// [(i, (j:jedges)), (j, (i:iedges))]
    where iedges = v V.! i
          jedges = v V.! j


-- get the vertices adjacent to vertex i
adj :: Graph -> Int -> [Int]
adj (Graph v) i = v V.! i


-- the total number of vertices in the graph
numVertices :: Graph -> Int
numVertices (Graph v) = V.length v


-- the total number of edges in the graph
numEdges :: Graph -> Int
numEdges (Graph v) = V.foldl' (\acc lst -> acc + length lst) 0 v

