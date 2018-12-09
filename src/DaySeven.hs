module DaySeven where

import qualified Data.Set as Set
import Data.List.Split (splitOn)
import Data.List (sort, partition, elemIndex)
import Data.Maybe (fromMaybe)

type Vertex = String
type Edge = (Vertex, Vertex)
data Graph = Graph { vertices :: Set.Set Vertex, edges :: Set.Set Edge } deriving (Eq, Show)
data Process = Process {vertex :: Vertex, timeRemaining :: Int}

empty = Graph { vertices=Set.empty, edges=Set.empty }

parseLine :: String -> Edge
parseLine line = 
    let 
        (x:y:_) = splitOn " must be finished before step " line
        fromId = head $ tail $ splitOn " " x
        toId = head $ splitOn " " y
    in 
        (fromId, toId)

registerEdge :: Edge -> Graph -> Graph
registerEdge (from, to) (Graph {vertices=v, edges=e}) = Graph {
        vertices = Set.union v $ Set.fromList [from, to],
        edges = Set.insert (from, to) e
    }

makeGraphFrom :: [Edge] -> Graph
makeGraphFrom = foldr registerEdge empty

roots :: Graph -> Set.Set Vertex
roots (Graph {vertices=v, edges=e}) = Set.difference v $ Set.map snd e

removeVertex :: Vertex -> Graph -> Graph
removeVertex r (Graph {vertices=v, edges=e}) = Graph { 
        vertices = Set.delete r v,
        edges = Set.filter (\(a, b) -> a /= r && b /= r) e 
    }

topologicalSort :: Graph -> [String] -> [String]
topologicalSort graph acc = 
    let 
        r = head $ sort $ Set.toList $ roots graph
        g = removeVertex r graph
    in 
        case g of 
            Graph {vertices=v} | v == Set.empty -> reverse (r:acc)
            _ -> topologicalSort g (r:acc)

timeRequired :: Vertex -> Int
timeRequired v = fromMaybe 0 (fmap (+61) $ elemIndex (head v) ['A'..'Z'])

processFrom :: Vertex -> Process
processFrom v = Process {vertex = v, timeRemaining = timeRequired v}

step :: Int -> Process -> Process
step by p = p {timeRemaining = timeRemaining p - by}

parallelizedTime :: Graph -> [Process] -> Int -> Int
parallelizedTime graph workers total
 | graph == empty = total
 | otherwise =
    let 
        (busy, free) = partition ((> 0) . timeRemaining) workers
        busyProcs = map vertex busy
        completed = map vertex free
        graph' = foldr removeVertex graph completed
        available = Set.filter (`notElem` busyProcs) $ roots graph'
        newProcesses = map processFrom $ take (length free) (Set.toAscList available ++ repeat " ")
        stepBy = case (length free, length busy) of
            (0, _) -> minimum $ map timeRemaining busy
            (_, 0) -> 0
            _      -> 1
    in 
        parallelizedTime graph' (map (step stepBy) (busy ++ newProcesses)) (total + stepBy)

partOne lines = foldl1 (++) $ topologicalSort (makeGraphFrom (map parseLine lines)) []
partTwo lines = parallelizedTime (makeGraphFrom (map parseLine lines)) (take 5 $ repeat (Process {vertex=" ", timeRemaining=0})) 0