import Test.QuickCheck
import System.Exit (exitSuccess, exitFailure)
import Control.Monad.Error

import qualified Graph.Test
import qualified DGraph.Test
import qualified PQ.Test
import qualified RBTree.Test
import qualified DepthFirstPaths.Test


type QuickCheckMonad = ErrorT String IO

quickCheckResultW :: (Testable prop) => prop -> QuickCheckMonad Result
quickCheckResultW prop = do
    result <- liftIO $ quickCheckResult prop
    case result of
        (Success _ _ _) -> return result
        (Failure {reason=str}) -> throwError str


main :: IO ()
main = do
    result <- runErrorT (quickCheckResultW Graph.Test.prop_num_edges >>
                         quickCheckResultW Graph.Test.prop_num_vertices >>
                         quickCheckResultW Graph.Test.prop_add_edge >> 
                         quickCheckResultW DGraph.Test.prop_num_edges >>
                         quickCheckResultW DGraph.Test.prop_num_vertices >>
                         quickCheckResultW DGraph.Test.prop_add_edge >> 
                         quickCheckResultW RBTree.Test.prop_rbtree_insert >>
                         quickCheckResultW PQ.Test.prop_pq_empty_size_0 >>
                         quickCheckResultW PQ.Test.prop_pq_max >>
                         quickCheckResultW PQ.Test.prop_pq_delmax >>
                         quickCheckResultW PQ.Test.prop_pq_insert)
    case result of
        Right _      -> exitSuccess
        Left _       -> exitFailure
