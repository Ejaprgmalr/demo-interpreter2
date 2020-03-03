-- Rudimentary test suite. Feel free to replace anything.

import Absyn
import Parser
-- import Elaborator
import Solver

import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck as QC

main :: IO ()
main = defaultMain $ localOption (mkTimeout 1000000) tests

-- tests = testGroup "Minimal tests" [
--   testCase "parser" $
--     parseString dbt @?= Right dbi,
--   testCase "elaborator" $
--     elaborate dbi @?= Right dbf,
--   testCase "solver" $
--     solve dbf goal 3 @?= Right sol
--   ]
--   where
--     dbt = "resource r. component c: provides r."

--     dbi = (["r"], [IC "c" [(CKProvides, RSRes "r")]])
--     dbf = ([R "r"], [("c", [(R "r", (1,0))])])

--     goal = [(R "r", (0,1))]
--     sol = [("c", 1)]





tests :: TestTree
tests = testGroup "Tests" [unitests_Parser, unitests_Solver]
  
-- quickcheck_Parser:: TestTree
-- quickcheck_Parser = testGroup "--------quickcheck for Parser------"
--    [ QC.testProperty "Resource Test 1" $
--        \s -> parseString ("resource "++(show (s::Int))++".") == Right ([show s], [])
--      -- QC.testProperty "operate 2" $
     --   \a b -> operate Minus (IntVal (a::Int)) (IntVal (b::Int)) == Right (IntVal (a-b)),
     -- QC.testProperty "operate 3" $
     --   \a b -> operate Times (IntVal (a::Int)) (IntVal (b::Int)) == Right (IntVal (a*b))
      -- ]


unitests_Parser :: TestTree
unitests_Parser = testGroup "---------Unit Tests for Parser----------"
  [testCase "Warmup Test 1: given in test framework" $
     parseString "resource r. component c : provides r." @?= Right (["r"], [IC "c" [(CKProvides, RSRes "r")]]),
   testCase "Warmup Test 2: givein in file goal" $
     parseString "resource PC. component DreamPC: provides PC;requires 12 GB-Ram;requires monitor;requires OS." @?= Right (["PC"],[IC "DreamPC" [(CKProvides,RSRes "PC"),(CKRequires,RSNum 12 (RSRes "GB-Ram")),(CKRequires,RSRes "monitor"),(CKRequires,RSRes "OS")]]),
   
   testCase "Priority Test 1 for RSpec: scalling operator tighter than ," $
     parseString "resource PC.component PC:requires monitor | 12 GB-Ram, OS." @?= Right (["PC"],[IC "PC" [(CKRequires,RSOr (RSRes "monitor") (RSAnd (RSNum 12 (RSRes "GB-Ram")) (RSRes "OS")))]]),
   testCase "Priority Test 2 for RSpec: , is tighter than |"$
     parseString "component PC: provides screen | moniter, USB, interface." @?= Right ([],[IC "PC" [(CKProvides,RSOr (RSRes "screen") (RSAnd (RSAnd (RSRes "moniter") (RSRes "USB")) (RSRes "interface")))]]),


   testCase "Associativity Test 1: , is left associative. " $
     parseString "component PC: provides moniter, USB, disk, camera." @?= Right ([],[IC "PC" [(CKProvides,RSAnd (RSAnd (RSAnd (RSRes "moniter") (RSRes "USB")) (RSRes "disk")) (RSRes "camera"))]]),
   testCase "Associativity Test 2: | is left associative. " $  
     parseString "component PC: provides moniter| USB| disk| camera." @?= Right ([],[IC "PC" [(CKProvides,RSOr (RSOr (RSOr (RSRes "moniter") (RSRes "USB")) (RSRes "disk")) (RSRes "camera"))]]),
   testCase "Associativity Test 3: scalling operator is right associative. " $  
     parseString "component PC: provides moniter, 4 USB, 3 camera." @?= Right ([],[IC "PC" [(CKProvides,RSAnd (RSAnd (RSRes "moniter") (RSNum 4 (RSRes "USB"))) (RSNum 3 (RSRes "camera")))]]),
   testCase "Associativity Test 4: a | 4 b , c == a | ((4 b), c)" $
     parseString "component PC: uses a | 4 b, c." @?= Right ([],[IC "PC" [(CKUses,RSOr (RSRes "a") (RSAnd (RSNum 4 (RSRes "b")) (RSRes "c")))]]),
   testCase "Associativity Test a, 3 b, 4 5 c == (a, 3 b), 4 (5 c)" $
     parseString "component PC: uses a, 3 b, 4 5 c." @?= Right ([],[IC "PC" [(CKUses,RSAnd (RSAnd (RSRes "a") (RSNum 3 (RSRes "b"))) (RSNum 4 (RSNum 5 (RSRes "c"))))]]),


   testCase "whitespace Test 1: tab in in the front" $
     parseString "\t resource PC.component PC:requires monitor | 12 GB-Ram, OS." @?= Right (["PC"],[IC "PC" [(CKRequires,RSOr (RSRes "monitor") (RSAnd (RSNum 12 (RSRes "GB-Ram")) (RSRes "OS")))]]),
   testCase "whitespace Test 2: tab in in the middle" $
     parseString "resource \tPC." @?= Right (["PC"], []),
   testCase "whitespace Test 3: tab in in the end" $
     parseString "resource PC.\t" @?= Right (["PC"], []),
   testCase "whitespace Test 4: newline in the front" $
     parseString "\n resource PC.component \nPC:requires monitor | 12 GB-Ram, OS." @?= Right (["PC"],[IC "PC" [(CKRequires,RSOr (RSRes "monitor") (RSAnd (RSNum 12 (RSRes "GB-Ram")) (RSRes "OS")))]]),
   testCase "whitespace Test 5: newline in the middle" $
     parseString "component PC: \n uses USB." @?= Right ([], [IC "PC" [(CKUses, RSRes "USB")]]),
   testCase "whitespace Test 6: newline in the end" $
     parseString "component PC: uses USB.\n" @?= Right ([], [IC "PC" [(CKUses, RSRes "USB")]]),
   testCase "whitespace Test 7 :spaces in the front" $
     parseString "    resource PC.component PC:requires monitor | 12 GB-Ram, OS." @?= Right (["PC"],[IC "PC" [(CKRequires,RSOr (RSRes "monitor") (RSAnd (RSNum 12 (RSRes "GB-Ram")) (RSRes "OS")))]]),
   testCase "whitespace Test 8 :spaces in the middle" $
     parseString "resource PC.    component PC:requires monitor | 12 GB-Ram, OS." @?= Right (["PC"],[IC "PC" [(CKRequires,RSOr (RSRes "monitor") (RSAnd (RSNum 12 (RSRes "GB-Ram")) (RSRes "OS")))]]),
   testCase "whitespace Test 9 :spaces in the end" $
     parseString "resource PC.component PC:requires monitor | 12 GB-Ram, OS.     " @?= Right (["PC"],[IC "PC" [(CKRequires,RSOr (RSRes "monitor") (RSAnd (RSNum 12 (RSRes "GB-Ram")) (RSRes "OS")))]]),

   testCase "Comment Test 1: nonnested comment in the end:" $
     parseString "resource PC.component PC:requires monitor | 12 GB-Ram, OS{this is comment}." @?= Right (["PC"],[IC "PC" [(CKRequires,RSOr (RSRes "monitor") (RSAnd (RSNum 12 (RSRes "GB-Ram")) (RSRes "OS")))]]),
   testCase "Comment Test 2: nested comment with adjacent curly brace in the end:" $
     parseString "resource PC.component PC:requires monitor | 12 GB-Ram, OS{{this is comment}}." @?= Right (["PC"],[IC "PC" [(CKRequires,RSOr (RSRes "monitor") (RSAnd (RSNum 12 (RSRes "GB-Ram")) (RSRes "OS")))]]),
   testCase "Comment Test 3: comment in the front:" $
     parseString "{this is comment}resource r." @?= Right (["r"], []),
   testCase "Comment Test 4: comment in the middle:" $
     parseString "resource {this is comment}r." @?= Right (["r"], []),

   testCase "IDB Structure Test 1: single resource" $
     parseString "resource r." @?= Right (["r"], []),
   testCase "IDB Strucutre Test 2: single component" $
     parseString "component c : provides r." @?= Right ([], [IC "c" [(CKProvides, RSRes "r")]]),
   testCase "IDB Structure Test 3: 1 resource followed by 1 component" $
     parseString "resource r.component c : provides r." @?= Right (["r"], [IC "c" [(CKProvides, RSRes "r")]]),
   testCase "IDB Structure Test 4: 1 component followed by 1 resource" $
     parseString "component c : provides r.resource r." @?= Right (["r"], [IC "c" [(CKProvides, RSRes "r")]]),
   testCase "IDB Structure Test 5: multi resource" $
     parseString "resource r. resource e. resource s." @?= Right (["r", "e", "s"], []),
   testCase "IDB Structure Test 6: multi component" $
     parseString "component c: provides usb. component c2: uses USB." @?= Right ([], [IC "c" [(CKProvides, RSRes "usb")], IC "c2" [(CKUses, RSRes "USB")]]) 
   -- testCase "IDB Structure Test 7: multiple resources and components appear alternately" $
   


  ] 




unitests_Solver :: TestTree
unitests_Solver = testGroup "---------Unit Tests for Solver----------"
  [testCase "combine Test 1: given in page 12" $
     combine [(R "A", (3, 5)), (R "C", (-2, 0)), (R "D", (3, 0))] [(R "A", (2, 7)), (R "B", (3, 4)), (R "D", (-3, 0))] @?= [(R "A", (5, 7)), (R "B", (3, 4)), (R "C", (-2, 0))],
   -- testCase "combine Test 2: "


   testCase "verify Test 1 : goal contains undeclared resource" $
     verify ([R "A", R "B"], [("ah", [(R "A", (1, 0))])]) [(R "C", (0, 1))] [("ah", 1)] @?= Left "goal contains database undeclased resource"
   testCase "verify Test 2 : " $
     verify ([R "A", R "B"], [("ah", [(R "A", (1, 0))])]) [(R "C", (0, 1))] [("ah", 1)] @?= Left "goal contains database undeclased resource"

  ]
