module ParserImpl where

import Absyn
-- other imports
import Text.ParserCombinators.ReadP as RP
import Data.Char


-- Code taken from previous assignment Parser:
parseString :: String -> Either String IDB
parseString str = let result = filter (\x -> snd x == "") (readP_to_S pIDB str) in
                  case result of
                     [] -> Left ("parse error on " ++ str)
                     (x:_) -> Right (fst x)

-- End of copied code
      

-- Code taken and modified from previous assignment2 and warmup:
token :: ReadP a -> ReadP a
token p = do
          skipSpaces
          skipMany pComment
          skipSpaces
          p         
-- End of copied code

pComment :: ReadP String
pComment = between (char '{') (char '}') (pComment +++ munch (/= '}'))


-- Code taken and modified from previous assignment2 and warmup :
symbol:: String  -> ReadP String
symbol str =  token $ string str
-- End of copied code


asciiLetter = ['A'..'Z'] ++ ['a'..'z']
asciiLetterDigit = ['A'..'Z'] ++ ['a'..'z'] ++ ['0'..'9']


letterFound :: String -> String -> Bool
letterFound _ [] = False
letterFound ascls (x:xs) = x `elem` ascls || letterFound ascls xs


nameLengthCheck :: String -> Bool
nameLengthCheck str = length str <= 32


-- Code taken from Internet, the 3rd anwser:
-- URL: https://stackoverflow.com/questions/9220986/is-there-any-haskell-function-to-concatenate-list-with-separator
joinBy :: String -> [String] -> String
joinBy sep cont = drop (length sep) $ concatMap (\w -> sep ++ w) cont
-- End of copied code



pWord :: ReadP String
pWord = token $
        do s <- munch1 (`elem` asciiLetterDigit)
           if letterFound asciiLetter s 
           then return s
           else fail "word should contain at least one letter"


pName :: ReadP String
pName = token $
        do n <- sepBy1 pWord (symbol "-")
           let str = joinBy "-" n
           if nameLengthCheck str
           then return str
           else fail "Name cannot be longer than 32 characters"
      

pNum :: ReadP Int
pNum  = do n <- munch1 isDigit
           if read n <= 999999 
           then return $ read n
           else fail "numeric value must be no more than 999999"
        

-- Code taken and modified from Internet, the 1st anwser:
-- https://stackoverflow.com/questions/16367150/how-to-make-the-parsec-chainl1-function-follow-operator-precedence-rules
pRSpec :: ReadP RSpec
pRSpec = pRSpec1 `chainl1` (do symbol "|"; return RSOr)


pRSpec1 :: ReadP RSpec
pRSpec1 = pRSpec2 `chainl1` (do symbol ","; return RSAnd)
-- End of copied code


pRSpec2 :: ReadP RSpec
pRSpec2 = token (between (char '(') (char ')') pRSpec)
            +++ (do n <- token pName
                    return $ RSRes n) 
            +++ (do n <-  token pNum
                    rspec <- token pRSpec2
                    return $ RSNum n rspec)
             

pClause :: ReadP Clause
pClause = (do symbol "provides"
              r <- token pRSpec
              return (CKProvides, r))
       +++(do symbol "uses"
              r <- token pRSpec
              return (CKUses, r))
       +++(do symbol "requires"
              r <- token pRSpec
              return (CKRequires, r))


pClauses :: ReadP [Clause]
pClauses = sepBy1 pClause (symbol ";")



pRNames :: ReadP [RName]
pRNames = do symbol "resource"
             a <- sepBy1 pName (symbol ",")
             symbol "."
             return a


pIComp :: ReadP IComp
pIComp = do symbol "component"
            cn <- pName
            symbol ":"
            cl <- pClauses
            symbol "."
            return $ IC cn cl

pIComps :: ReadP [IComp]
pIComps = sepBy1 pIComp (symbol "")


pResOrComp :: ReadP IDB
pResOrComp = (do rns <- token pRNames
                 return (rns, []))
          +++(do comp <- token pIComp
                 return ([], [comp]))


pIDB :: ReadP IDB
pIDB = do a <- token $ RP.many pResOrComp 
          skipSpaces
          skipMany pComment
          skipSpaces
          return $ merge a


merge :: [IDB] -> IDB
merge a = 
  case a of
       ((name, comp) : rest) -> 
                let from_rest = merge rest in
                (name ++ fst from_rest, comp ++ snd from_rest)
       [] -> ([],[])