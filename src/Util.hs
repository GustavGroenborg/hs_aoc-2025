module Util where

import System.Environment

exampleInputPath :: String
exampleInputPath = "exampleInputs/"

puzzleInputPath :: String
puzzleInputPath  = "puzzleInputs/"

getExampleInput :: String -> IO [String]
getExampleInput str = getInput $ exampleInputPath ++ str

getPuzzleInput :: String -> IO [String]
getPuzzleInput str = getInput $ puzzleInputPath ++ str

getInput :: String -> IO [String]
getInput path = do
    aocPath <- getEnv "AOC_PATH"
    content <- readFile $ aocPath ++ "/" ++ path
    return $ lines content
