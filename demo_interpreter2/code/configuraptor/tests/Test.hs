-- Rudimentary test suite. Feel free to replace anything.

import Absyn
import Parser
import Elaborator
import Solver

import Test.Tasty
import Test.Tasty.HUnit

main :: IO ()
main = defaultMain $ localOption (mkTimeout 1000000) tests

tests = testGroup "Minimal tests" [
  testCase "parser" $
    parseString dbt @?= Right dbi,
  testCase "elaborator" $
    elaborate dbi @?= Right dbf,
  testCase "solver" $
    solve dbf goal 3 @?= Right sol
  ]
  where
    dbt = "resource r. component c: provides r."
    dbi = (["r"], [IC "c" [(CKProvides, RSRes "r")]])
    dbf = ([R "r"], [("c", [(R "r", (1,0))])])
    goal = [(R "r", (0,1))]
    sol = [("c", 1)]
