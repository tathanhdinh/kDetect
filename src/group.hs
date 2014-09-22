-- =============================================================================
-- Copyright monads (monads@inbox.com) (INPL - INRIA - LORIA).
--   
-- Distributed under the Boost Software License.
-- =============================================================================
-- Description:
--   structure the execution trace of observed program
--     
-- Arguments:
--   raw interactions
--     
-- Returns:
--   trace
-- =============================================================================

import qualified Text.ParserCombinators.Parsec as Parsec
import qualified Text.Printf as Printf
import qualified Control.Monad as M
import qualified Control.Monad.State as S
import qualified Control.Monad.Trans as M
import qualified Data.List as L
import qualified System.Exit as E


-- -----------------------------------------------------------------------------
-- interaction parsed from a raw line
-- -----------------------------------------------------------------------------
data CapturedInteraction = CapturedInteraction { sysName :: String
                                               , fPath :: String
                                               , procFd :: Int
                                               , sysFd :: Integer
                                               , bPos :: Int
                                               , ePos :: Int 
                                               } 

                                               
-- -----------------------------------------------------------------------------
-- show a captured interaction
-- -----------------------------------------------------------------------------
instance Show CapturedInteraction where 
  show capIntr = Printf.printf "%-62s %-12d %-7d %-7s %-7d %-7d" 
                 (fPath capIntr) (sysFd capIntr) (procFd capIntr) 
                 (sysName capIntr) (bPos capIntr) (ePos capIntr)
                 

-- -----------------------------------------------------------------------------
-- sequential execution trace
-- -----------------------------------------------------------------------------
data SequentialTrace = SequentialTrace { seqTrFPath :: [String]
                                       , seqTrSysFd :: Integer
                                       , seqTrProcFd :: [Int]
                                       , seqTrSysNames :: [String]
                                       , seqTrFPos :: [(Int, Int)]
                                       } 

                                       
-- -----------------------------------------------------------------------------
-- show a sequential trace
-- -----------------------------------------------------------------------------
instance Show SequentialTrace where 
  show seqTr = "\n" ++ (show $ seqTrSysFd seqTr) ++ " " ++ (show $ seqTrProcFd seqTr)
               ++ " " ++ (show $ seqTrSysNames seqTr) ++ " " ++ (show $ seqTrFPos seqTr)
               ++ (show $ seqTrFPath seqTr) ++ "\n"

               
-- -----------------------------------------------------------------------------
-- trace operators
-- -----------------------------------------------------------------------------
class TraceOperators trace where 
  isEqual :: trace -> trace -> Bool
  isNotEqual :: trace -> trace -> Bool
  merge :: trace -> trace -> Maybe trace
  isSub :: trace -> trace -> Bool
  
  isEqual trace0 trace1 = not (isNotEqual trace0 trace1)
  isNotEqual trace0 trace1 = not (isEqual trace0 trace1)
  
  
-- -----------------------------------------------------------------------------
-- operator instances of a sequential trace
-- -----------------------------------------------------------------------------
instance TraceOperators SequentialTrace where 
  isEqual trace0 trace1 = 
    ((seqTrSysFd trace0) == (seqTrSysFd trace1))
    
  merge trace0 trace1 = 
    if (isEqual trace0 trace1)
    then Just SequentialTrace { seqTrFPath = seqTrFPath(trace0) ++ seqTrFPath(trace1)
                         , seqTrSysFd = seqTrSysFd(trace0)
                         , seqTrProcFd = seqTrProcFd(trace0) ++ seqTrProcFd(trace1)
                         , seqTrSysNames =  seqTrSysNames(trace0) ++ seqTrSysNames(trace1) 
                         , seqTrFPos = seqTrFPos(trace0) ++ seqTrFPos(trace1)
                         }
    else Nothing
--     else SequentialTrace { seqTrFPath = ["unknow"]
--                          , seqTrSysFd = 0
--                          , seqTrProcFd = []
--                          , seqTrSysNames = []
--                          , seqTrFPos = [] 
--                          }
                         
  isSub trace0 trace1 = L.isInfixOf (seqTrSysNames trace0) (seqTrSysNames trace1)
  
  
-- -----------------------------------------------------------------------------
-- parallel trace
-- -----------------------------------------------------------------------------
type Trace = [SequentialTrace]


-- -----------------------------------------------------------------------------
-- main function with error handler
-- -----------------------------------------------------------------------------
main :: IO ()
main = mainExecute `catch` errorHandler


-- -----------------------------------------------------------------------------
-- main procedure
-- -----------------------------------------------------------------------------
mainExecute :: IO ()
mainExecute = do 
--   lastTrace <- S.runStateT foreverPrintTrace []
--   putStrLn $ show lastTrace
  S.evalStateT foreverPrintTrace []
--   S.evalStateT foreverTest []
  
 
-- -----------------------------------------------------------------------------
-- test trace with a malware pattern 
-- hardcoded to detect logkeys (see at http://code.google.com/p/logkeys/)
-- -----------------------------------------------------------------------------
testTrace :: S.StateT Trace IO ()
testTrace = do
  currTrace <- S.get
  let logkeysTrace = SequentialTrace { seqTrFPath = []
                                     , seqTrSysFd = 0
                                     , seqTrProcFd = []
                                     , seqTrSysNames = ["read","read","read","read","read"]
                                     , seqTrFPos = []
                                     }
  if L.any (\x -> isSub logkeysTrace x) currTrace 
  then do 
    M.liftIO $ putStrLn "Bingo!!!" 
    M.liftIO $ E.exitSuccess
  else M.return ()
  
  
-- -----------------------------------------------------------------------------
-- test until find something or the capturing program finishes
-- -----------------------------------------------------------------------------
foreverTest :: S.StateT Trace IO ()
foreverTest = M.forever $ do 
  readIntrAndUpdateTrace
  testTrace

  
-- -----------------------------------------------------------------------------
-- forever print updated trace
-- -----------------------------------------------------------------------------
foreverPrintTrace :: S.StateT Trace IO ()
foreverPrintTrace = M.forever $ do 
  readIntrAndUpdateTrace
  printTrace
  
  
-- -----------------------------------------------------------------------------
-- update an execution trace structure with a new captured interaction
-- -----------------------------------------------------------------------------
readIntrAndUpdateTrace :: S.StateT Trace IO () 
readIntrAndUpdateTrace = do 
  rawIntr <- M.liftIO $ getLine
  currTrace <- S.get
  let newSeqTrace = (intrToTrace $ parseRawLine rawIntr)
  case (L.find (\x -> (isEqual x newSeqTrace)) currTrace) of 
    Nothing -> S.put $ currTrace ++ [newSeqTrace]
    Just seqTrace -> let filteredTrace = L.filter (\x -> (isNotEqual x newSeqTrace)) currTrace
                     in case merge seqTrace newSeqTrace of 
                          Just mergedTrace -> S.put $ filteredTrace ++ [mergedTrace]
--                        S.put $ filteredTrace ++ [(merge seqTrace newSeqTrace)]
  where intrToTrace :: CapturedInteraction -> SequentialTrace
        intrToTrace capIntr = 
          SequentialTrace 
            { seqTrFPath = [fPath(capIntr)]
            , seqTrSysFd = sysFd(capIntr)
            , seqTrProcFd = [procFd(capIntr)]
            , seqTrSysNames = [sysName(capIntr)]
            , seqTrFPos = [(bPos(capIntr), ePos(capIntr))] 
            }

            
-- -----------------------------------------------------------------------------         
-- print out the current trace
-- -----------------------------------------------------------------------------
printTrace :: S.StateT Trace IO ()
printTrace = do 
  currTrace <- S.get
  M.liftIO $ putStrLn $ show currTrace


-- -----------------------------------------------------------------------------
-- forever read an input line and parse it
-- -----------------------------------------------------------------------------
foreverReadAndParse :: IO ()
foreverReadAndParse = M.forever $ do 
  readAndParse
  
  
-- -----------------------------------------------------------------------------
-- error handler in case of IO error
-- -----------------------------------------------------------------------------
errorHandler :: IOError -> IO ()
errorHandler e = putStrLn "Observation finishes."
  
  
-- -----------------------------------------------------------------------------
-- read data from the standard input and parse the received raw line
-- -----------------------------------------------------------------------------
readAndParse :: IO ()
readAndParse = getLine >>= (\x -> putStrLn (show (parseRawLine x)))
  
  
-- -----------------------------------------------------------------------------
-- parse a raw input as strings
-- -----------------------------------------------------------------------------
parseRawLine :: String -> CapturedInteraction
parseRawLine input = 
  case (Parsec.parse lineParser "" input) of 
    Left err -> 
      CapturedInteraction { sysName = "unknow"
                          , fPath = "unknow"
                          , procFd = 0
                          , sysFd = 0
                          , bPos = 0
                          , ePos = 0
                          }
    Right xs -> 
      case xs of 
           [f1, f2, f3, f4, f5, f6] -> 
             CapturedInteraction { sysName = f1
                                 , fPath = f2
                                 , procFd = (read f3)::Int
                                 , sysFd = (read f4)::Integer
                                 , bPos = (read f5)::Int
                                 , ePos = (read f6)::Int
                                 }
           _ -> 
             CapturedInteraction { sysName = "unknow"
                                 , fPath = "unknow"
                                 , procFd = 0
                                 , sysFd = 0
                                 , bPos = 0
                                 , ePos = 0
                                 }


-- -----------------------------------------------------------------------------
-- rewrite a line parser
-- -----------------------------------------------------------------------------
lineParser :: Parsec.Parser [String]
lineParser = cellParser >>= (\x -> (nextCellsParser >>= (\y -> return (x:y))))


-- -----------------------------------------------------------------------------
-- parse the content of a cell
-- -----------------------------------------------------------------------------
cellParser :: Parsec.Parser String
cellParser = Parsec.many (Parsec.noneOf ",")


-- -----------------------------------------------------------------------------
-- parse a line with considering to ',' character at the beginning of line
-- -----------------------------------------------------------------------------
nextCellsParser :: Parsec.Parser [String]
nextCellsParser = 
  (Parsec.char ',' >> lineParser) 
  Parsec.<|> (return [])
