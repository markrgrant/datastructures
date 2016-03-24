{-# LANGUAGE TemplateHaskell #-}

import Test.QuickCheck
import System.Exit (exitSuccess, exitFailure)
import Control.Monad (liftM)

import qualified Graph.Test
import qualified PQ.Test
import qualified RBTree.Test
import qualified DepthFirstPaths.Test


import Data.List (foldl')


checkFailure :: Result -> Maybe ()
checkFailure Failure {} = Nothing
checkFailure _ = Just ()

main :: IO ()
main = quickCheckResult Graph.Test.prop_num_edges >>
       quickCheckResult Graph.Test.prop_num_vertices >>
       quickCheckResult Graph.Test.prop_add_edge >> 
       quickCheckResult RBTree.Test.prop_rbtree_insert >>
       quickCheckResult PQ.Test.prop_pq_empty_size_0 >>
       quickCheckResult PQ.Test.prop_pq_max >>
       quickCheckResult PQ.Test.prop_pq_delmax >>
       quickCheckResult PQ.Test.prop_pq_insert >>
       return ()
