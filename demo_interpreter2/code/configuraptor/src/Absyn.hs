-- Do not modify anything in this file!

module Absyn where

-- Intermediate database
type IDB = ([RName], [IComp])

data RSpec =
    RSRes RName
  | RSNum Int RSpec
  | RSAnd RSpec RSpec
  | RSOr RSpec RSpec
  deriving (Eq, Show, Read)

type Clause = (CKind, RSpec)

data CKind = CKProvides | CKUses | CKRequires
  deriving (Eq, Show, Read)

data IComp = IC CName [Clause]
  deriving (Eq, Show, Read)

type RName = String
type CName = String

type ErrMsg = String

-- Final database

type DB = ([Resource], [(CName, RProf)])

type RProf = [(Resource, (Int, Int))] -- (net contribution, requirement)

newtype Resource = R String
  deriving (Eq,Ord,Show,Read)

-- Solver-related definitions

type Goal = RProf

type Solution = [(CName, Int)] 
