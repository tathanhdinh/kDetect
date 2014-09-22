-- -----------------------------------------------------------------------------
-- Copyright TA Thanh Dinh (monads@inbox.com) (INPL - INRIA - LORIA).
--   
-- Distributed under the Boost Software License.
-- -----------------------------------------------------------------------------
-- Description:
--   malware detectors
--     
-- Arguments:
--   trace
--     
-- Returns:
--   malware detected signal or nothing
-- -----------------------------------------------------------------------------

module Detectors (detectMalware) where

import Trace
import qualified Control.Monad      as M
import qualified Data.List          as L
import qualified System.Exit        as E
import qualified Control.Concurrent as C


-- -----------------------------------------------------------------------------
-- main malware detector
-- -----------------------------------------------------------------------------
detectMalware :: Trace -> IO ()
detectMalware trace = do 
  C.forkIO $ do detectLogkeys trace
  C.forkIO $ do detectSmall trace
  M.return ()
  

-- -----------------------------------------------------------------------------
-- detect Linux keylogger (see at http://code.google.com/p/logkeys/)
-- -----------------------------------------------------------------------------
detectLogkeys :: Trace -> IO ()
detectLogkeys trace = 
  let logkeysTrace = SequentialTrace { seqTrFPath = "/input/event"
                                     , seqTrSysFd = 0
                                     , seqTrProcFd = []
                                     , seqTrSysNames = L.replicate 10 "read"
                                     , seqTrFPos = []
                                     , seqTrProcId = []
                                     } 
  in if L.any (\x -> isSub logkeysTrace x) trace 
     then do 
       putStrLn "Input device scanning detected (maybe a keylogger)." 
       E.exitSuccess
     else M.return ()
  
-- -----------------------------------------------------------------------------
-- detect Small.o trojan (in the collection of Fabrique)
-- -----------------------------------------------------------------------------
detectSmall :: Trace -> IO ()
detectSmall trace = 
  let smallTrace = SequentialTrace { seqTrFPath = "/2"
                                   , seqTrSysFd = 0
                                   , seqTrProcFd = []
                                   , seqTrSysNames = L.replicate 7 "close"
                                   , seqTrFPos = []
                                   , seqTrProcId = []
                                   }
  in if L.any (\x -> isSub smallTrace x) trace 
     then do 
       putStrLn "Massive close at the same stderr detected (maybe a fork server trojan)."
       E.exitSuccess
     else M.return ()
  
  
-- -----------------------------------------------------------------------------
-- verify subtrace
-- -----------------------------------------------------------------------------
isSub :: SequentialTrace -> SequentialTrace -> Bool
isSub trace0 trace1 = (L.isInfixOf (seqTrSysNames trace0) (seqTrSysNames trace1)) &&
                      (L.isInfixOf (seqTrFPath trace0) (seqTrFPath trace1))