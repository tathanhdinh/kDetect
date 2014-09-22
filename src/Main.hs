-- -----------------------------------------------------------------------------
-- Copyright TA Thanh Dinh (monads@inbox.com) (INPL - INRIA - LORIA).
--   
-- Distributed under the Boost Software License.
-- -----------------------------------------------------------------------------
-- Description:
--   main program and error handler
-- -----------------------------------------------------------------------------

module Main (main) where 

import Trace     (Trace, readInteractionAndUpdateTrace)
import Detectors (detectMalware)

import qualified Control.Monad       as M
import qualified Control.Monad.State as S
import qualified Control.Monad.Trans as T


-- -----------------------------------------------------------------------------
-- entry point
-- -----------------------------------------------------------------------------
main :: IO ()
main = mainExecute `catch` errorHandler


-- -----------------------------------------------------------------------------
-- main procedure
-- -----------------------------------------------------------------------------
mainExecute :: IO ()
mainExecute = do 
--   S.evalStateT foreverTestTrace []
  S.evalStateT foreverPrintTrace []

  
-- -----------------------------------------------------------------------------
-- test until find something or the capturing program finishes
-- -----------------------------------------------------------------------------
foreverTestTrace :: S.StateT Trace IO ()
foreverTestTrace = M.forever $ do 
  readInteractionAndUpdateTrace
  currentTrace <- S.get
  T.liftIO $ detectMalware currentTrace
  

-- -----------------------------------------------------------------------------
-- just print out current trace
-- -----------------------------------------------------------------------------
foreverPrintTrace :: S.StateT Trace IO ()
foreverPrintTrace = M.forever $ do 
  readInteractionAndUpdateTrace
  currentTrace <- S.get
  T.liftIO $ putStrLn $ show currentTrace
  
  
-- -----------------------------------------------------------------------------
-- error handler in case of IO error
-- -----------------------------------------------------------------------------
errorHandler :: IOError -> IO ()
errorHandler e = putStrLn "Observation finishes."
