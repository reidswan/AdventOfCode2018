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
import qualified DayTen
import qualified DayEleven
import qualified DayTwelve
import qualified DayThirteen
import qualified DayFourteen

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
    let (players, limit) = (427, 70723)
        soln1 = DayNine.partOne players limit
    putStrLn ("Part one: " ++ show soln1)
    let soln2 = DayNine.partOne players (100*limit)
    putStrLn ("Part two: " ++ show soln2)


dayTen :: IO ()
dayTen = do
    putStrLn "Day 10"
    contents <- fileContents "input/dayTen.txt"
    let points = map DayTen.parseLine $ filter (not . null) contents
    DayTen.solution 0 points

dayEleven :: IO ()
dayEleven = do
    putStrLn "Day 11"
    let grid = DayEleven.powerGrid 9995
        soln1 = DayEleven.part1 grid
    putStrLn ("Part one: " ++ show soln1)
    let soln2 = DayEleven.part2 grid -- this uses like 2GB of RAM?? 
    putStrLn ("Part two: " ++ show soln2)

dayTwelve :: IO ()
dayTwelve = do
    putStrLn "Day 12"
    (first:rest) <- fileContents "input/dayTwelve.txt"
    let initState = DayTwelve.parseInitialState first
        rules = DayTwelve.makeRules rest
        soln1 = DayTwelve.part1 rules initState
    putStrLn ("Part one: " ++ show soln1)
    let soln2 = DayTwelve.part2 rules initState
    putStrLn ("Part two: " ++ show soln2)

dayThirteen :: IO ()
dayThirteen = do
    putStrLn "Day 13"
    lines <- fileContents "input/dayThirteen.txt"
    let soln1 = DayThirteen.partOne lines
    putStrLn ("Part one: " ++ show soln1)
    let soln2 = DayThirteen.partTwo lines
    putStrLn ("Part two: " ++ show soln2)

dayFourteen :: IO ()
dayFourteen = do
    putStrLn "Day 14"
    end <- readLn :: IO Int
    let result = DayFourteen.go end DayFourteen.init
    print result

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
        (putStrLn "Days are 1-indexed!"), dayOne, (putStrLn "TODO: migrate Day 2 from Rust to Haskell"),
        dayThree, dayFour, dayFive, daySix, daySeven, dayEight, dayNine, dayTen, dayEleven, dayTwelve,
        dayThirteen, dayFourteen
    ]
