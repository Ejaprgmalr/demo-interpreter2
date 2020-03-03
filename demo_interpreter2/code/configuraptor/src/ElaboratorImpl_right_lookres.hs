module ElaboratorImpl where

import Absyn
-- add other imports
import Data.Char

import Control.Monad
import qualified Data.Map as Map
import Data.Map(Map)
import Control.Applicative

import Data.List




-- modify canonically 
-- lookres :: [Resource] -> RName -> Either ErrMsg Resource
-- lookres [] _ = Left "not found resource res"
-- lookres  ((R curr) : rest) target =
--     case toUpperString curr == toUpperString target of
--         True -> Right (R curr)
--         False -> lookres rest target




lookres :: [Resource] -> RName -> Either ErrMsg Resource
lookres [] _ = Left "not found resource res"
lookres  ((R curr) : rest) target =
    case curr == target of
        True -> Right (R $ toUpperString curr)
        False -> lookres rest target


lookcns :: [Resource] -> CName -> Either ErrMsg Resource
lookcns [] _ = Left "not found resource cns"
lookcns  ((R curr) : rest) target =
    case curr == target of
        True -> Right (R curr)
        False -> lookcns rest target




toUpperString :: [Char] -> [Char]
toUpperString = map toUpper





-- Int: provides, useds, requireds
type REnv = Map Resource (Int, Int, Int)
type REnvUnit = (Resource, (Int, Int, Int))

data CEnv = CENV (Map CName REnv)
     deriving (Eq,Ord,Show,Read)


type ContentUnit = (RName, Int)
data Content = CONTENT [ContentUnit]
     deriving (Eq,Ord,Show,Read)

type StandardClauseUnit = (CKind, RName, Int)
data StandardClause = STDCLAU [StandardClauseUnit]
     deriving (Eq,Show,Read)


type StandardComponentUnit = (CName, StandardClause)
type StandardComponent = [StandardComponentUnit]

type StandardRProf = [(CName, RProf)]


newtype Elaba a = Elaba {runElaba :: [Resource] -> (Either ErrMsg a, [Resource])}

-- instance Monad Elaba where
--   return a = Elaba (\env -> (return a, env))
--   m >>= f = Elaba (\env -> (do
--                               (a, env') <- runElaba m env
--                               runElaba (f a) env'))
--   fail s = Elaba (\_ -> Left s)

instance Monad Elaba where
  return a = Elaba (\env -> (Right a, env))
  m >>= f = Elaba $ \env -> case runElaba m env of
                             (Left err, s) -> (Left err, s)
                             (Right a, s) -> case runElaba (f a) env of
                                               (Left err, s') -> (Left err, s++s')
                                               (Right b, s') -> (Right b, s++s')
  fail s = Elaba (\e -> (Left s, e))


instance Functor Elaba where
  fmap = liftM
instance Applicative Elaba where
  pure = return; (<*>) = ap

-- 
initElab :: [Resource] -> Elaba ()
initElab resc = Elaba (\_ -> (Right (), resc))


getRes :: Elaba [Resource]
getRes = Elaba (\env -> (Right env, env))


check_res :: RName -> Elaba RName
check_res rname =  Elaba (\rs -> case (lookres rs rname) of
                          Left err -> (Left (err++(show rs)), rs)
                          Right upperName -> (Right (toUpperString rname), rs))


check_cns :: CName -> Elaba CName
check_cns cname = do
  rs <- Elaba (\env -> (Right env, env))
  case lookcns rs cname of
    Left err -> return $ cname
    Right upperName -> fail "conflict cname with resource names"




-- 

buildResourceMap :: StandardClause -> REnv -> REnv
buildResourceMap (STDCLAU []) container = container
buildResourceMap (STDCLAU ((ckind, rn, i):rest)) container =
    case Map.lookup (R rn) container of
        Nothing ->
            case ckind of
                CKProvides -> let newContainer = Map.insert (R rn) (i, 0, 0) container in
                              buildResourceMap (STDCLAU rest) newContainer
                CKUses     -> let newContainer = Map.insert (R rn) (0, i, 0) container in
                              buildResourceMap (STDCLAU rest) newContainer
                CKRequires -> let newContainer = Map.insert (R rn) (0, 0, i) container in
                              buildResourceMap (STDCLAU rest) newContainer

        Just (proi, usei, reqi) ->
            case ckind of
                CKProvides -> let newContainer = Map.insert (R rn) (proi+i, usei, reqi) container in
                              buildResourceMap (STDCLAU rest) newContainer
                CKUses     -> let newContainer = Map.insert (R rn) (proi, usei+i, reqi) container in
                              buildResourceMap (STDCLAU rest) newContainer
                CKRequires -> let newContainer = Map.insert (R rn) (proi, usei, reqi+i) container in
                              buildResourceMap (STDCLAU rest) newContainer

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








elabName :: RSpec -> Elaba Content
elabName (RSNum i (RSRes rn)) = do
  upName <- check_res rn
  case upName of
    upname -> return $ CONTENT [(upname, i)]
    _ -> fail "invalid resource name"
elabName (RSRes rn) = do
  upName <- check_res rn
  case upName of
    upname -> return $ CONTENT [(upname, 1)]
    _ -> fail "invalid resource name"
elabName (RSAnd rsp1 rsp2) = do
  CONTENT fst_list <- elabName rsp1
  CONTENT snd_list <- elabName rsp2
  return $ CONTENT (fst_list ++ snd_list)
elabName (RSOr rsp1 rsp2) = do
  CONTENT fst_list <- elabName rsp1
  CONTENT snd_list <- elabName rsp2
  case (fst_list, snd_list) of
    ([(r1, 1)], [(r2, 1)]) -> return $ CONTENT [(r1++"or"++r2, 1)]
    ([(r1, i1)], [(r2, i2)]) -> return $ CONTENT [(show i1 ++ r1++"or"++show i2 ++r2, 1)]
    _ -> fail "invalid RSOr input"

elabName _ = fail "invalid form of RSpec"



elabClauses :: [Clause] -> [StandardClauseUnit] -> Elaba StandardClause
elabClauses [] standardClause = return $ STDCLAU standardClause
elabClauses ((ckind, rspec):rest) standardClause = do
  name_result <- elabName rspec
  case name_result of
    CONTENT content_list -> elabClauses rest (standardClause ++ (add_ckind ckind content_list []))
    _ -> fail "invalid Clauses"

add_ckind :: CKind -> [ContentUnit] -> [StandardClauseUnit] -> [StandardClauseUnit]
add_ckind _ [] container = container
add_ckind ckind ((rn, i):rest) container = add_ckind ckind rest ((ckind, rn, i):container)



buildCompMap :: [IComp] -> CEnv -> Elaba CEnv
buildCompMap [] container = return container
buildCompMap ((IC cn clause_list):rest) (CENV container) =
        case Map.lookup cn container of
            Nothing    -> do
              env <- getRes
              stdcla <- (elabClauses clause_list [])
              -- case (lookcns env cn) of
                case check_cns cn of
                   
                Right (R resource) -> buildCompMap rest (CENV (Map.insert resource (buildResourceMap stdcla Map.empty) container))
                Left err -> fail err
            Just renv -> (do
              STDCLAU clause <- elabClauses clause_list []
              buildCompMap rest (CENV (Map.insert cn (buildResourceMap (STDCLAU clause) renv) container)) )


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


calcRProf :: [IComp] -> Elaba StandardRProf
calcRProf icomps = do
  CENV cenv <- buildCompMap icomps (CENV Map.empty)
  return (transferMap2RProf (Map.toList cenv) [])


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


-- keeplook :: [Resource] -> RProf -> Either ErrMsg [Resource]
-- keeplook _ [] = Right []
-- keeplook (R rn) ((reso, intpair) : rest) =
--     case lookres resource reso of
--       Left err-> Left err
--       Right rightRes -> keeplook resource rest


-- checkUndeclare :: [Resource] -> [(CName, RProf)] -> Either ErrMsg Resource
-- checkUndeclare resources [] = Right []
-- checkUndeclare resources ((cname, rprof) : rest) =
--     case lookres resources cname of
--         Right _ -> Left "resource and component name conflix"
--         Left _ -> case keeplook resource  of
--                   Left err -> Left err
--                   Right _ -> checkUndeclare resource rest





eval_elaborate :: IDB -> Elaba DB
eval_elaborate (reso, icomps) = do
  _ <- initElab (addR reso)
  stdRProf <- calcRProf icomps
  return ((addR reso), stdRProf)

elaborate :: IDB -> Either ErrMsg DB
elaborate idb@(reso, icomps) = 
    case (runElaba (eval_elaborate idb) (addR reso)) of
        (Left err, env) -> Left err
        (Right standardRProf, env) -> Right standardRProf
