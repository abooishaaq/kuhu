module Main where

import Parser (parseToplevel, toplevel)
import System.Environment (getArgs)
import Text.Pretty.Simple (pPrint)

import Env (empty)

main :: IO ()
main = do
    args <- getArgs
    case args of
        [name] ->
            readFile name >>= \contents -> do
                case parseToplevel contents of
                    Left err -> print err
                    Right toplevel -> pPrint toplevel
        _ -> putStrLn "Usage: kuhu <filename>"
