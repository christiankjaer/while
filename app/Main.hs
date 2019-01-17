module Main where

import Parser
import CFG
import Data.Graph.Inductive.Graph
import System.Environment

main :: IO ()
main = do
    (f:_) <- getArgs
    prog <- parseFile f
    prettyPrint $ compile prog
