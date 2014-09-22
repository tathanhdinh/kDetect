-- -----------------------------------------------------------------------------
-- Copyright TA Thanh Dinh (monads@inbox.com) (INPL - INRIA - LORIA).
--   
-- Distributed under the Boost Software License.
-- -----------------------------------------------------------------------------
-- Description:
--   trace definition and parsed functions
-- -----------------------------------------------------------------------------

module Trace ( SequentialTrace(..)
             , TraceOperators(..)
             , Trace
             , readInteractionAndUpdateTrace
             ) where

             
import qualified Text.ParserCombinators.Parsec as Parsec
import qualified Text.Printf                   as Printf
import qualified Control.Monad                 as M
import qualified Control.Monad.State           as S
import qualified Control.Monad.Trans           as T
import qualified Data.List                     as L
import qualified System.Exit                   as E


-- -----------------------------------------------------------------------------
-- interaction parsed from a raw line
-- -----------------------------------------------------------------------------
data CapturedInteraction = CapturedInteraction { procId :: Int 
                                               , sysName :: String
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
  show capIntr = Printf.printf "%-7d %-62s %-12d %-7d %-7s %-7d %-7d" 
                 (procId capIntr) (fPath capIntr) (sysFd capIntr) (procFd capIntr) 
                 (sysName capIntr) (bPos capIntr) (ePos capIntr)
                 
                 
-- -----------------------------------------------------------------------------
-- sequential execution trace
-- -----------------------------------------------------------------------------
data SequentialTrace = SequentialTrace { seqTrFPath :: String
                                       , seqTrSysFd :: Integer
                                       , seqTrProcFd :: [Int]
                                       , seqTrSysNames :: [String]
                                       , seqTrFPos :: [(Int, Int)]
                                       , seqTrProcId :: [Int]
                                       }
                                       

-- -----------------------------------------------------------------------------
-- show a sequential trace
-- -----------------------------------------------------------------------------
instance Show SequentialTrace where 
  show seqTr = "\n" ++ (show $ seqTrProcId seqTr) ++ " " ++ (show $ seqTrSysFd seqTr) 
               ++ " " ++ (show $ seqTrProcFd seqTr)++ " " ++ (show $ seqTrSysNames seqTr) 
               ++ " " ++ (show $ seqTrFPos seqTr) ++ " " ++ (show $ seqTrFPath seqTr) ++ "\n"
               
               
-- -----------------------------------------------------------------------------
-- trace operators
-- -----------------------------------------------------------------------------
class TraceOperators trace where 
  isEqual :: trace -> trace -> Bool
  isNotEqual :: trace -> trace -> Bool
  merge :: trace -> trace -> Maybe trace
  
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
    then let traces = [trace0, trace1] 
         in Just SequentialTrace { seqTrFPath    = seqTrFPath(trace0)
                                 , seqTrSysFd    = seqTrSysFd(trace0)
                                 , seqTrProcFd   = L.concatMap seqTrProcFd   traces
                                 , seqTrSysNames = L.concatMap seqTrSysNames traces
                                 , seqTrFPos     = L.concatMap seqTrFPos     traces
                                 , seqTrProcId   = L.concatMap seqTrProcId   traces
                                 }
    else Nothing
  

-- -----------------------------------------------------------------------------
-- parallel trace
-- -----------------------------------------------------------------------------
type Trace = [SequentialTrace]


-- -----------------------------------------------------------------------------
-- convert a captured interaction to a simple trace
-- -----------------------------------------------------------------------------
interactionToTrace :: CapturedInteraction -> SequentialTrace
interactionToTrace capIntr = 
  SequentialTrace { seqTrFPath    = fPath(capIntr)
                  , seqTrSysFd    = sysFd(capIntr)
                  , seqTrProcFd   = [procFd(capIntr)]
                  , seqTrSysNames = [sysName(capIntr)]
                  , seqTrFPos     = [(bPos(capIntr), ePos(capIntr))]
                  , seqTrProcId   = [procId(capIntr)]
                  }
                  

-- -----------------------------------------------------------------------------
-- update an execution trace structure with a new captured interaction
-- -----------------------------------------------------------------------------
readInteractionAndUpdateTrace :: S.StateT Trace IO () 
readInteractionAndUpdateTrace = do 
  rawIntr <- T.liftIO $ getLine
  currTrace <- S.get
  let newSeqTrace = (interactionToTrace $ parseRawLine rawIntr)
  case (L.find (\x -> (isEqual x newSeqTrace)) currTrace) of 
    Nothing -> 
      S.put $ currTrace ++ [newSeqTrace]
    Just seqTrace -> 
      let filteredTrace = L.filter (\x -> (isNotEqual x newSeqTrace)) currTrace
      in case merge seqTrace newSeqTrace of 
        Just mergedTrace -> 
          S.put $ filteredTrace ++ [mergedTrace]
        Nothing -> 
          S.return ()
        
--   where intrToTrace :: CapturedInteraction -> SequentialTrace
--         intrToTrace capIntr = 
--           SequentialTrace { seqTrFPath    = fPath(capIntr)
--                           , seqTrSysFd    = sysFd(capIntr)
--                           , seqTrProcFd   = [procFd(capIntr)]
--                           , seqTrSysNames = [sysName(capIntr)]
--                           , seqTrFPos     = [(bPos(capIntr), ePos(capIntr))] 
--                           }
                          
                          
-- -----------------------------------------------------------------------------
-- parse a raw input as strings
-- -----------------------------------------------------------------------------
parseRawLine :: String -> CapturedInteraction
parseRawLine input = 
  case (Parsec.parse lineParser "" input) of 
    Left err -> 
      CapturedInteraction { procId  = 0
                          , sysName = "unknow"
                          , fPath   = "unknow"
                          , procFd  = 0
                          , sysFd   = 0
                          , bPos    = 0
                          , ePos    = 0
                          }
    Right xs -> 
      case xs of 
           [f0, f1, f2, f3, f4, f5, f6] -> 
             CapturedInteraction { procId  = (read f0)::Int
                                 , sysName = f1
                                 , fPath   = f2
                                 , procFd  = (read f3)::Int
                                 , sysFd   = (read f4)::Integer
                                 , bPos    = (read f5)::Int
                                 , ePos    = (read f6)::Int
                                 }
           _ -> 
             CapturedInteraction { procId  = 0
                                 , sysName = "unknow"
                                 , fPath   = "unknow"
                                 , procFd  = 0
                                 , sysFd   = 0
                                 , bPos    = 0
                                 , ePos    = 0
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
  