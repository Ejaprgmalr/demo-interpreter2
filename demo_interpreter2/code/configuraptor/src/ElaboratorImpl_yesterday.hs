module ElaboratorImpl where

import Absyn
-- add other imports
import Data.Char

import Control.Monad
import qualified Data.Map as Map
import Data.Map(Map)
import Control.Applicative

import Data.List


lookres :: [Resource] -> RName -> Either ErrMsg Resource
lookres [] _ = Left "not found resource"
lookres  ((R curr) : rest) target =
    case curr == target of
        True -> Right (R $ toUpperString curr)
        False -> lookres rest target


toUpperString :: [Char] -> [Char]
toUpperString = map toUpper





-- Int: provides, useds, requireds
type REnv = Map Resource (Int, Int, Int)
type REnvUnit = (Resource, (Int, Int, Int))

type CEnv = Map CName REnv

newtype Elaba a = Elaba {runElaba :: String -> (Either ErrMsg (a, [Resource]))}

instance Monad Elaba where
  return a = Elaba $ \env -> (Right a, env)
  m >>= f = Elaba (\env -> (do
                              (a, env') <- runElaba m env
                              runElaba (f a) env'))
  fail s = Elaba (\_ -> Left s)


instance Functor Elaba where
  fmap = liftM
instance Applicative Elaba where
  pure = return; (<*>) = ap



type ContentUnit = (RName, Int)
type Content = [ContentUnit]

type StandardClauseUnit = (CKind, RName, Int)
type StandardClause = [StandardClauseUnit]


type StandardComponentUnit = (CName, StandardClause)
type StandardComponent = [StandardComponentUnit]

type StandardRProf = [(CName, RProf)]


buildResourceMap :: StandardClause -> REnv -> REnv
buildResourceMap [] container = container
buildResourceMap ((ckind, rn, i):rest) container = 
    case Map.lookup (R rn) container of
        Nothing -> 
            case ckind of
                CKProvides -> let newContainer = Map.insert (R rn) (i, 0, 0) container in
                              buildResourceMap rest newContainer 
                CKUses     -> let newContainer = Map.insert (R rn) (0, i, 0) container in
                              buildResourceMap rest newContainer
                CKRequires -> let newContainer = Map.insert (R rn) (0, 0, i) container in
                              buildResourceMap rest newContainer

        Just (proi, usei, reqi) ->
            case ckind of
                CKProvides -> let newContainer = Map.insert (R rn) (proi+i, usei, reqi) container in
                              buildResourceMap rest newContainer
                CKUses     -> let newContainer = Map.insert (R rn) (proi, usei+i, reqi) container in
                              buildResourceMap rest newContainer
                CKRequires -> let newContainer = Map.insert (R rn) (proi, usei, reqi+i) container in
                              buildResourceMap rest newContainer

mergeRMap :: [REnvUnit] -> REnv -> REnv
mergeRMap [] renv = renv
mergeRMap ((resource, (proi, usei, reqi)) : rest) renv =
    case Map.lookup resource renv of
        Nothing -> 
                let newRenv = Map.insert resource (proi, usei, reqi) renv in
                mergeRMap rest newRenv
        Just (proi1, usei1, reqi1) -> 
                let newRenv = Map.insert resource (proi + proi1, usei + usei1, reqi + reqi1) renv in
                mergeRMap rest newRenv




elabName :: RSpec -> Either ErrMsg Content
elabName (RSNum i (RSRes rn)) = Right [(rn, i)]
elabName (RSRes rn) = Right [(rn, 1)]
elabName (RSAnd rsp1 rsp2) = 
    case (elabName rsp1, elabName rsp2) of
        (Right fst_list, Right snd_list) -> Right (fst_list ++ snd_list)  
        (Left _, _) -> Left "err"
        (_, Left _)   -> Left " err"
elabName (RSOr rsp1 rsp2) = 
    case (elabName rsp1, elabName rsp2) of
        (Right [(r1, 1)], Right [(r2, 1)]) -> Right [(r1++"or"++r2, 1)]
        (Right [(r1, i1)], Right [(r2, i2)]) -> Right [(show i1 ++ r1++"or"++show i2 ++r2, 1)]
        (Left _, _) -> Left "err"
        (_, Left _)   -> Left "err"
elabName _ = Left "invalid form of RSpec"


elabClauses :: [Clause] -> StandardClause -> Either ErrMsg StandardClause
elabClauses [] standardClause = Right standardClause
elabClauses ((ckind, rspec):rest) standardClause =
    case (elabName rspec) of
        (Right content_list) -> 
               elabClauses rest (standardClause ++ (add_ckind ckind content_list []))
        (Left err) -> Left err

add_ckind :: CKind -> Content -> StandardClause -> StandardClause
add_ckind _ [] container = container
add_ckind ckind ((rn, i):rest) container = add_ckind ckind rest ((ckind, rn, i):container)



buildCompMap :: [IComp] -> CEnv -> Either ErrMsg CEnv
buildCompMap [] container = Right container
buildCompMap ((IC cn clause_list):rest) container =
        case Map.lookup cn container of
            Nothing    -> case elabClauses clause_list [] of
                              Right standardClause -> 
                                  let new_container = Map.insert cn (buildResourceMap standardClause Map.empty) container in
                                  buildCompMap rest new_container
                              Left err -> Left err

            Just renv  -> case elabClauses clause_list [] of
                            Right standardClause ->
                                  let thisREnv = buildResourceMap standardClause renv
                                      new_container = Map.insert cn thisREnv container
                                  in  buildCompMap rest new_container
                            Left err -> Left err


transferMap2RProf :: [(CName, REnv)] -> StandardRProf -> StandardRProf
transferMap2RProf [] standardRProf = standardRProf
transferMap2RProf ((cn, renv):rest) standardRProf = 
          let currRProf = getRProf (Map.toList renv) [] in
              transferMap2RProf rest ((cn, currRProf):standardRProf)




getRProf :: [REnvUnit] -> RProf -> RProf
getRProf [] rprof = rprof
getRProf ((resource, (proi, usei, reqi)):rest) rprof= 
           case (proi-usei, reqi) of
                (0, 0)    -> getRProf rest rprof
                (cn, req) -> case req > usei of
                              True -> getRProf rest ((resource, (cn, req)):rprof)
                              False -> getRProf rest ((resource, (cn, usei)):rprof)


calcRProf :: [IComp] -> Either ErrMsg StandardRProf
calcRProf icomps = case buildCompMap icomps Map.empty of
    Left err -> Left err
    Right cenv -> Right (transferMap2RProf (Map.toList cenv) [])  


-- icomp_init :: [IComp] -> Elaba ()
-- icomp_init icomps =
--         Elaba(\_ -> case (calcRProf icomps) of
--             Left err -> Left err
--             Right standardRProf -> Right ((), standardRProf))

-- get_standardRProf :: Elaba () -> Elaba StandardRProf
-- get_standardRProf a =
--     Elaba(\std -> Right std)


-- check_res :: IDB -> Elaba DB
-- check_res idb@(rnames, icomps) = 
--     case icomp_init icomps of
--         err -> Left ([], [])
        -- Right standardRProf -> 

sortAndUpper :: [RName] -> [RName]
sortAndUpper rns = map toUpperString (sort rns)

addR :: [RName]  -> [Resource]
addR rns = map (\rn -> R rn) rns

removeR :: RProf -> [RName]
removeR rns = map (\(R rn, intpair) -> toUpperString rn) rns

upperKey :: RProf -> RProf
upperKey rprof = map (\(R rn, intpair) -> (R (toUpperString rn), intpair)) rprof

constructRProf :: [RName] -> Map Resource (Int, Int) -> RProf
constructRProf rnames oneMap =
    case rnames of
        [] -> []
        (rname : rest) -> case Map.lookup (R rname) oneMap of
                               Nothing -> []
                               Just intPair -> [(R rname, intPair)] ++ (constructRProf rest oneMap)

polishRProf :: (CName, RProf) -> (CName, RProf)
polishRProf stdRProfUnit@(cname, rprof) =
    let 
        rns = removeR rprof
        theMap = Map.fromList (upperKey rprof)
        sortedRns = sort rns
        newrprof = constructRProf sortedRns theMap
    in
        (cname, newrprof)


    

-- elaborate :: IDB -> Either ErrMsg DB
-- elaborate (reso, icomps) =
--     case calcRProf icomps of
--         Left err -> Left err
--         Right standardRProf -> Right (addR $ sortAndUpper reso, (map polishRProf standardRProf))



-- lookres :: [Resource] -> RName -> Either ErrMsg Resource
-- lookres [] _ = Left "not found resource"
-- lookres  ((R curr) : rest) target =
--     case curr == target of
--         True -> Right (R $ toUpperString curr)
--         False -> lookres rest target

keeplook :: [Resource] -> RProf -> Either ErrMsg [Resource]
keeplook _ [] = Right []
keeplook (R rn) ((reso, intpair) : rest) =
    case lookres resource reso of
      Left err-> Left err
      Right rightRes -> keeplook resource rest 


checkUndeclare :: [Resource] -> [(CName, RProf)] -> Either ErrMsg Resource
checkUndeclare resources [] = Right []
checkUndeclare resources ((cname, rprof) : rest) =
    case lookres resources cname of
        Right _ -> Left "resource and component name conflix"
        Left _ -> case keeplook resource  of
                  Left err -> Left err
                  Right _ -> checkUndeclare resource rest





elaborate :: IDB -> Either ErrMsg DB
elaborate (reso, icomps) =
    case calcRProf icomps of
        Left err -> Left err
        Right standardRProf -> lookres (addR $ sortAndUpper reso, (map polishRProf standardRProf))
