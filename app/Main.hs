module Main where

import Parser (parseToplevel, toplevel)
import System.Environment (getArgs)
import Text.Pretty.Simple (pPrint)

import Check (runTopLevelTys)
import Env (empty)
import Infer (initInfer)

main :: IO ()
main = do
    args <- getArgs
    case args of
        [name] ->
            readFile name >>= \contents -> do
                case parseToplevel contents of
                    Left err -> print err
                    Right toplevel -> do
                        pPrint toplevel
                        pPrint $ runTopLevelTys toplevel
                        return ()
        _ -> putStrLn "Usage: kuhu <filename>"
