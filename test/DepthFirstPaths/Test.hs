module DepthFirstPaths.Test where

import Test.QuickCheck
import qualified Graph as G
import qualified Graph.Test
import qualified DepthFirstPaths as P


instance Arbitrary P.Paths where
    arbitrary = do
        graph <- arbitrary
        vertex <- choose (0, (G.numVertices graph) - 1)
        return $ P.create graph vertex
