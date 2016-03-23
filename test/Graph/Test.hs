module Graph.Test where

import Test.QuickCheck
import qualified Graph as G


instance Arbitrary G.Graph where
    arbitrary = do
        n <- choose (1,2) :: Gen Int
        case n of
            1 -> do -- create a graph
                numEdges <- choose (0, 100)
                return $ G.create numEdges
            2 -> do -- add an edge to a graph
                g <- arbitrary
                fr <- choose (0, (G.numVertices g) - 1)
                to <- choose (0, (G.numVertices g) - 1)
                return $ G.addEdge g fr to


-- Adding an edge to a graph increases its edge count by 1
prop_num_edges :: G.Graph -> Bool
prop_num_edges g
    | numVertices == 0 = True -- can't add any edges
    | otherwise = G.numEdges g' == (G.numEdges g + 1)
    where numVertices = G.numVertices g
          g' = G.addEdge g 0 (numVertices-1)


-- The number of vertices is the same as the number provided when creating the
-- graph
prop_num_vertices :: Int -> Bool
prop_num_vertices n = G.numVertices g == n
    where g = G.create n
    

-- Adding an edge from vertex i to vertex j makes vertex j
-- adjacent to vertex i, and vertex i adjacent to vertex j
prop_add_edge :: Bool
prop_add_edge = 0 `elem` (G.adj g' 9) && 9 `elem` (G.adj g' 0)
    where g = G.create 10
          g' = G.addEdge g 0 9
