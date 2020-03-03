module Main where

import Absyn
import Parser
import Elaborator
import Solver

import System.Environment (getArgs)
import Text.Printf (printf)
import System.Exit (die)

getAns :: String -> Either String a -> IO a
getAns s (Right a) = return a
getAns s (Left e) = die $ "Failure for " ++ s ++ ": " ++ e

procDB :: [String] -> IDB -> IO (IDB, [String])
procDB (('@':fname):as) (rs,cs) =
  do s <- readFile fname
     (rs1,cs1) <- getAns ("parsing " ++ fname) $ parseString s
     procDB as (rs++rs1, cs++cs1)
procDB (('%':fname):as) (rs,cs) =
  do s <- readFile fname
     let (rs1,cs1) = read s :: IDB
     procDB as (rs++rs1, cs++cs1)
procDB as idb = return (idb,as)

procQ :: [String] -> [Resource]  -> (Int, Goal) -> IO (Int, Goal)
procQ [] _rs (n,g) = return (n,g)
procQ ("-n":ns:as) rs (n,g) =
  let n1 = read ns :: Int
  in procQ as rs (n1, g)
procQ ("-p":ns:r:as) rs (n,g) =
  do let n1 = read ns :: Int
     r1 <- getAns "-p" $ lookres rs r
     procQ as rs (n, combine g [(r1, (n1,0))])
procQ ("-r":ns:r:as) rs (n,g) =
  do let n1 = read ns :: Int
     r1 <- getAns "-r" $ lookres rs r 
     procQ as rs (n, combine g [(r1, (0,n1))])
procQ as _ _ = die $ "bad args at " ++ show as

showSol :: Solution -> String
showSol sol =
  concatMap (\(c,n) -> printf "%4d %s\n" n c) sol

showRProf :: RProf -> String
showRProf rp =
  concatMap (\(R r, (n, _)) -> printf "%4d %s\n" n r) rp

main =
  do as <- getArgs
     (idb, as1) <- procDB as ([], [])
     db@(rs,_) <- getAns "elaborating" $ elaborate idb
     (n, g) <- procQ as1 rs (10, [])
     sol <- getAns "solving" $ solve db g n
     rp <- getAns "verifying" $ verify db g sol
     putStrLn $ "Success! Suggested configuration:\n" ++ showSol sol
     putStrLn $ "Available resources:\n" ++ showRProf rp
  
