module Util where

basePath :: String
basePath         = "/Users/gustavgronborg/AdventOfCode/aoc_2025/"

exampleInputPath :: String
exampleInputPath = basePath ++ "exampleInputs/"

puzzleInputPath :: String
puzzleInputPath  = basePath ++ "puzzleInputs/"

getExampleInput :: String -> IO [String]
getExampleInput str = getInput $ exampleInputPath ++ str

getPuzzleInput :: String -> IO [String]
getPuzzleInput str = getInput $ puzzleInputPath ++ str

getInput :: String -> IO [String]
getInput path = do
    content <- readFile $ path
    return $ lines content
