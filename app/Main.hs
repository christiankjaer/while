module Main where

import Parser
import CFG
import Analysis
import Data.Graph.Inductive.Graph
import System.Environment

main :: IO ()
main = do
    (f:_) <- getArgs
    prog <- parseFile f
    putStrLn "Program\n-------"
    print prog
    putStrLn "\nCFG\n-------"
    let p = compile prog
    prettyPrint p
    putStrLn "\nTrue liveness\n-------"
    print $ getTl p
    putStrLn "\nAvailable expressions\n-------"
    print $ getAe p
