-- Utility functions for retrieving information
-- about a Graph.
module Graph.Util (
  degree, 
  maxDegree,
  avgDegree,
  numberOfSelfLoops
) where

import qualified Graph as G


-- Return the maximum degree of the vertex in average time proportional
-- to the average degree of the graph. 
degree :: G.Graph -> Int -> Int
degree g v = length $ G.adj g v


-- Return the maximum degree of all vertices in the graph in time
-- proportional to V+E.
maxDegree :: G.Graph -> Int
maxDegree g = maximum $ map (degree g) [0..(G.numVertices g -1)]


-- Return the average degree of the graph in time proportional
-- to V+E.
avgDegree :: G.Graph -> Double
avgDegree g = 2 * fromIntegral(G.numEdges g) / fromIntegral (G.numVertices g)


-- Return the number of self loops in the graph in time
-- proportional to V+E.
numberOfSelfLoops :: G.Graph -> Int
numberOfSelfLoops g = sum $ map selfCounts [0..(G.numVertices g - 1)]
  where selfCounts v = sum $ map (\v'-> if v' == v then 1 else 0) $ G.adj g v

