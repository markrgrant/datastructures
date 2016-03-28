module DGraph.Test where

import Test.QuickCheck
import qualified DGraph as G
import Data.List (sort)


instance Arbitrary G.DGraph where
    arbitrary = do
        n <- choose (1,2) :: Gen Int
        case n of
            1 -> do -- create a graph with at least one vertex
                numVertices <- choose (1, 100)
                return $ G.create numVertices
            2 -> do -- add an edge to a graph
                g <- arbitrary
                fr <- choose (0, G.numVertices g - 1)
                to <- choose (0, G.numVertices g - 1)
                return $ G.addEdge g fr to


-- Adding an edge to a graph increases its edge count by 1
prop_num_edges :: G.DGraph -> Bool
prop_num_edges g = G.numEdges g' == (G.numEdges g + 1)
    where g' = G.addEdge g 0 (G.numVertices g - 1)


-- The number of vertices is the same as the number provided when creating the
-- graph
prop_num_vertices :: NonNegative Int -> Bool
prop_num_vertices (NonNegative n) = G.numVertices g == n
    where g = G.create n
    

-- Adding an edge from vertex i to vertex j makes vertex j
-- adjacent to vertex i, but vertex i is not adjacent to vertex j
prop_add_edge :: Bool
prop_add_edge = 9 `elem` G.adj g' 0 && 0 `notElem` G.adj g' 9
    where g = G.create 10
          g' = G.addEdge g 0 9


-- The edge list of the reverse of a graph is the list of edges of the 
-- graph whose edges have been reversed
prop_reverse :: G.DGraph -> Bool
prop_reverse g = gEdges1 == gEdges2
    where gEdges1 = sort $ G.toList (G.reverse g)
          gEdges2 = sort $ map (\(a,b) -> (b,a)) (G.toList g)

