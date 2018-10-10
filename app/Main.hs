module Main where

import ATPPFHS
-- import TryMap

main :: IO ()
main = do
    putStr $ show ax1
    putStrLn " : Axiom 1"
    putStr $ show ax2
    putStrLn " : Axiom 2."
    putStr $ show ax3
    putStrLn " : Axiom 3."
    putStr $ show (normalize ax3)
    putStrLn " : Axiom 3, “normalized”."
    
