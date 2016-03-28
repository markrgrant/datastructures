-- A type for the analysis of connectedness of vertices
-- in a Graph. 
--
-- > import qualified Graph as G
-- > import qualified BreadthFirstPaths as P
--
-- > let g = G.fromList 10 [(0,1),(0,2),(2,3),(4,7),(2,8)]
-- > let paths = P.create g 0
-- > P.hasPathTo 8
-- True
-- > P.pathTo 8
-- [0,2,8]


module BreadthFirstPaths (
    create,
    hasPathTo,
    pathTo
) where


import qualified Data.Vector as V
import qualified Graph as G
import Data.List (foldl')
import Data.Maybe (fromJust, isNothing, isJust)


data Paths = Paths (V.Vector (Maybe Int))


-- Create a new breadth-first search tree from the given graph and
-- vertex that can be used to determine the existence of a path and
-- the contents of a path between the vertex and any other vertex.
create :: G.Graph -> Int -> Paths
create graph root = Paths $ bfs graph [root] tree'
    where tree = V.replicate (G.numVertices graph) Nothing
          tree' = tree V.// [(root, Just root)]


-- Recursively do a breadth-first search of a graph, constructing a tree
-- of visited vertices.
bfs :: G.Graph -> [Int] -> V.Vector (Maybe Int) -> V.Vector (Maybe Int)
bfs graph level tree
    | null level = tree -- done constructing tree
    | otherwise = bfs graph level' tree'
    where (level', tree') = foldl' visit ([], tree) level
          visit (l, t) v = (l', t')
              where isUnvisited vert = isNothing (t V.! vert)
                    adj = filter isUnvisited (G.adj graph v)
                    t' = t V.// zip (repeat v) (map Just adj)
                    l' = adj ++ l


-- Determine if the given vertex is connected to the root, in constant
-- time.
--
-- The vertex is connected to the root vertex if the vertex is in the
-- depth-first search tree of the vertex.  The root vertex is considered
-- connected having a path to itself.
hasPathTo :: Paths -> Int -> Bool
hasPathTo (Paths tree) v = isJust $ tree V.! v


-- Find a path to the root vertex in time proportional to the
-- path length.   The path begins with the vertex used to create
-- the paths instance and ends with the given vertex.
pathTo :: Paths -> Int -> [Int]
pathTo (Paths tree) v = pathTo' v tree []


pathTo' :: Int -> V.Vector (Maybe Int) -> [Int] -> [Int]
pathTo' cur edges xs
    | cur == parent = cur:xs -- done
    | otherwise = pathTo' parent edges (cur:xs)
    where parent = fromJust $ edges V.! cur

