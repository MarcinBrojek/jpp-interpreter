module Main where

import Control.Monad

import System.Environment
import System.Exit
import System.IO

import AbsGrammar
import ParGrammar
import LexGrammar

import TypeChecker as TC
import Interpreter as IP
import ErrM

main :: IO ()
main = do
    args <- getArgs
    case args of
        [] -> do
            contents <- getContents
            runContents contents
        [prog] -> do
            handle <- openFile prog ReadMode
            contents <- hGetContents handle
            runContents contents
        _ -> hPutStr stderr "Wrong number of arguments - expected one"

runAll :: String -> Err String
runAll txt = case pProgram (myLexer txt) of
    Ok tree -> case TC.run tree of
        Bad err -> Bad ("Type checker error: " ++ err)
        _ -> case IP.run tree of
            Ok res -> Ok res
            Bad err -> Bad ("Interpreter error: " ++ err)
            _ -> Ok "" -- avoid incomplete pattern
    _ -> Bad "Parse error"

runContents :: String -> IO ()
runContents contents = case runAll contents of
    Ok res -> putStr res
    Bad err -> hPutStr stderr err
    _ -> putStr "" -- avoid incomplete pattern