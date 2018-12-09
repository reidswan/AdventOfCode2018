module Main where

import System.IO
import System.Environment (getArgs)
import Control.Monad
import qualified DayOne
import qualified DayThree
import qualified DayFour
import qualified DayFive
import qualified DaySix
import qualified DaySeven
import qualified DayEight
import qualified DayNine

main :: IO ()
main = do
    args <- getArgs
    if null args
        then putStrLn "Please supply a day to run"
        else days !! read (head args)


dayOne :: IO ()
dayOne = do
    putStrLn "Day 1"
    fileLines <- fileContents "input/dayOne.txt"
    let numbers = asNumbers fileLines
    let soln1 = DayOne.partOne numbers
    putStrLn ("Part one: " ++ show soln1)
    case DayOne.partTwo numbers of 
        (Just x) -> putStrLn ("Part two: " ++ show x)
        Nothing -> putStrLn "Part two: no solution"


dayThree :: IO ()
dayThree = do
    putStrLn "Day 3"
    fileLines <- fileContents "input/dayThree.txt"
    let claims = [DayThree.parseLine line | line <- fileLines]
        soln1 = DayThree.partOne claims
        soln2 = DayThree.partTwo claims
    putStrLn ("Part one: " ++ show soln1)
    putStrLn ("Part two: " ++ show soln2)


dayFour :: IO ()
dayFour = do
    putStrLn "Day 4"
    fileLines <- fileContents "input/dayFour.txt"
    let shifts = DayFour.parseLines fileLines
        soln1 = DayFour.partOne shifts
        soln2 = DayFour.partTwo shifts
    putStrLn ("Part one: " ++ show soln1)
    putStrLn ("Part two: " ++ show soln2)


dayFive :: IO ()
dayFive = do
    putStrLn "Day 5"
    line <- firstFileLine "input/dayFive.txt"
    let soln1 = DayFive.partOne line
    putStrLn ("Part one: " ++ show soln1)
    let soln2 = DayFive.partTwo line
    putStrLn ("Part two: " ++ show soln2)

daySix :: IO ()
daySix = do
    putStrLn "Day 6"
    src <- fileContents "input/daySix.txt"
    let lines = map DaySix.parseLine src
    let soln1 = DaySix.partOne lines
    putStrLn ("Part one: " ++ show soln1)
    let soln2 = DaySix.partTwo lines
    putStrLn ("Part two: " ++ show soln2)

daySeven :: IO ()
daySeven = do
    putStrLn "Day 7"
    lines <- fileContents "input/daySeven.txt"
    let soln1 = DaySeven.partOne lines
    putStrLn ("Part one: " ++ show soln1)
    let soln2 = DaySeven.partTwo lines
    putStrLn ("Part two: " ++ show soln2)

dayEight :: IO ()
dayEight = do
    putStrLn "Day 8"
    lines <- firstFileLine "input/dayEight.txt"
    let soln1 = DayEight.partOne lines
    putStrLn ("Part one: " ++ show soln1)
    let soln2 = DayEight.partTwo lines
    putStrLn ("Part two: " ++ show soln2)

dayNine :: IO ()
dayNine = do
    putStrLn "Day 9"
    let (players, limit) = (427, 7072300) -- lazy
        soln1 = DayNine.partOne players limit
    putStrLn ("Part one: " ++ show soln1)


firstFileLine :: [Char] -> IO [Char]
firstFileLine filename = do
    handle <- openFile filename ReadMode
    hGetLine handle


fileContents :: [Char] -> IO [[Char]]
fileContents filename = do
    handle <- openFile filename ReadMode
    contents <- hGetContents handle
    return $ lines contents

asNumbers :: [[Char]] -> [Integer]
asNumbers = map (read . dropWhile (== '+'))

days = 
    [
        (putStrLn "Days are 1-indexed!"), dayOne, dayThree,
        dayFour, dayFive, daySix, daySeven, dayEight, dayNine
    ]

