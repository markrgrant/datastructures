{-# LANGUAGE TemplateHaskell #-}

import Test.QuickCheck
import System.Exit (exitSuccess, exitFailure)

import qualified Graph.Test
import qualified PQ.Test
import qualified RBTree.Test
import qualified DepthFirstPaths.Test


import Data.List (foldl')

-- Create an arbitrary Paths instance from an arbitrary Graph
-- and an arbitrary vertex within that graph as the starting point.

main :: IO ()
main = do
    result <- quickCheckResult Graph.Test.prop_num_edges
    case result of
        (Success _ _ _) -> exitSuccess
        _ -> exitFailure
