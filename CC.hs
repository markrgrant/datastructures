module CC (
    create,
    count,
    id
) where


import Prelude hiding (id)
import qualified Data.Vector as V
import Data.List (foldl')
import Data.Maybe (fromJust)

import qualified Graph as G

data CC = CC (V.Vector Int) Int


-- Calculates the connected components of the graph.
create :: G.Graph -> CC
create graph = CC comps'' count'
    where count = 0
          numVertices = (G.numVertices graph)
          comps = V.replicate numVertices Nothing
          (comps', count') = foldl' (findcc graph) (comps, count) [0..(numVertices-1)]
          comps'' = V.map fromJust comps' -- every vertex is in a component
           

-- Assign the given vertex to a connected component.
--
-- If it is already in a connected component, don't do anything.
-- Otherwise, increment the # of connected components, and do a depth-first
-- search at the vertex to find the other vertices that are part of the
-- same connected component and assign them to the component as well.
findcc :: G.Graph -> (V.Vector (Maybe Int), Int) -> Int -> (V.Vector (Maybe Int), Int)
findcc graph (comps, count) vertex
    | comps V.! vertex /= Nothing = (comps, count) -- vertex already in a comp
    | otherwise = (comps', count') -- vertex not yet in a comp
    where count' = count+1
          comps' = dfs graph count comps vertex -- complete the component


-- perform dfs in order to flesh out the component:
--   add the vertex to the component
--   perform dfs on the adjacent vertices
dfs :: G.Graph -> Int -> (V.Vector (Maybe Int)) -> Int -> (V.Vector (Maybe Int))
dfs graph count comps vertex = foldl' (dfs graph count) comps' adj
    where comps' = comps V.// [(vertex, Just count)]
          isUnvisited v = (comps V.! v) == Nothing
          adj = filter isUnvisited (G.adj graph vertex)


-- The number of connected components in the  graph, in constant time.
count :: CC -> Int
count (CC _ c) = c


-- The id of the connected component of the vertex, in constant time.
id :: CC -> Int -> Int
id (CC cs _) v = cs V.! v
