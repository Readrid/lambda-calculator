{-# LANGUAGE InstanceSigs #-}
import Data.List
import Text.ParserCombinators.Parsec

type Symb = String 

infixl 2 :@
infix 1 `alphaEq`

data Expr = Var Symb
          | Expr :@ Expr
          | Lam Symb Expr
          deriving (Eq)

freeVars :: Expr -> [Symb]
freeVars (Var s) = [s]
freeVars (e1 :@ e2) = nub $ (freeVars e1) ++ (freeVars e2)
freeVars (Lam s e)  = filter (/= s) (freeVars e)

subst :: Symb -> Expr -> Expr -> Expr 
subst v n (Var s)    | v == s    = n
                     | otherwise = Var s
subst v n (e1 :@ e2) = subst v n e1 :@ subst v n e2
subst v n (Lam y e)  | v == y    = Lam y e
                     | otherwise = case (find (== y) (freeVars n)) of
                                   Just y  -> Lam (newVar y) (subst v n (subst y (Var $ newVar y) e))
                                   Nothing -> Lam y $ subst v n e
                                   where newVar x = getNewVar (freeVars n ++ freeVars e) x 

getNewVar :: [Symb] -> Symb -> Symb
getNewVar xs x = case find (== x) xs of
                 Just _  -> getNewVar xs (x ++ "'")
                 Nothing -> x

alphaEq :: Expr -> Expr -> Bool
alphaEq (Var x) (Var y)         = x == y
alphaEq (e1 :@ e2) (e1' :@ e2') = (alphaEq e1 e1') && (alphaEq e2 e2') 
alphaEq (Lam x e1) (Lam y e2)   | x == y    = alphaEq e1 e2
                                | otherwise = case find (== x) (freeVars e2) of
                                              Nothing -> alphaEq (Lam x e1) (Lam x (subst y (Var x) e2))
                                              Just _  -> False
alphaEq _          _            = False

reduceOnce :: Expr -> Maybe Expr
reduceOnce ((Lam s e) :@ x) = Just (subst s x e)
reduceOnce (Lam s e)        = case reduceOnce e of
                              Just expr -> Just $ Lam s expr
                              Nothing   -> Nothing
reduceOnce (e1 :@ e2)       = case reduceOnce e1 of
                              Just expr -> Just (expr :@ e2)
                              Nothing   -> case reduceOnce e2 of
                                           Just expr1 -> Just (e1 :@ expr1)
                                           Nothing    -> Nothing   
reduceOnce (Var a)          = Nothing

nf :: Expr -> Expr 
nf e = case reduceOnce e of
            Just expr -> nf expr
            Nothing   -> e

betaEq :: Expr -> Expr -> Bool 
betaEq e1 e2 = nf e1 `alphaEq` nf e2

instance Show Expr where
  show :: Expr -> String
  show (Var s) = s
  show ((Var a) :@ (Var b)) = a ++ " " ++ b
  show ((Var a) :@ e)       = a ++ " " ++ "(" ++ show e ++ ")"
  show (e@(Lam _ _) :@ (Var a)) = "(" ++ show e ++ ") " ++ a
  show (e :@ (Var a))           = show e ++ " " ++ a
  show (e1@(t1 :@ t2) :@ e2)    =  show e1 ++ " (" ++ show e2 ++ ")"
  show (e1 :@ e2)               = "(" ++ show e1 ++ ") (" ++ show e2 ++ ")"
  show lam = "\\" ++ myConcat ++ show nextExpr
    where lamvar = fst $ getLamVars lam
          nextExpr = snd $ getLamVars lam
          myConcat = foldr (\a b -> a ++ " " ++ b) "-> " lamvar

getLamVars :: Expr -> ([Symb], Expr)
getLamVars (Lam x e) = case e of 
  Lam s' e' -> (x : (fst $ getLamVars e), snd $ getLamVars e)
  e         -> ([x], e)

instance Read Expr where
  readsPrec _ s = case parseString s of
    Right e -> [(e, "")]
    Left _ -> [] 

parseString :: String -> Either ParseError Expr
parseString =
  parse (do r <- parseExpr; eof; return r) ""

parseVar :: Parser Expr
parseVar = fmap Var parseSymb

parseSymb :: Parser String
parseSymb = do
  h <- (letter <|> char '_')
  t <- many (alphaNum <|> char '_')
  return (h:t)

parseApl :: Parser Expr
parseApl = do
  exprs <- sepBy parseTerms spaces
  return $ foldl1 (:@) exprs

parseTerms :: Parser Expr
parseTerms = parseVar <|> parseScopedExpr  

parseScopedExpr :: Parser Expr
parseScopedExpr = do
  spaces
  char '('
  e <- parseExpr
  char ')'
  spaces
  return e

parseLam :: Parser Expr
parseLam = do
  char '\\'
  x <- many1 (parseSymb <* spaces)
  char '-'
  char '>'
  spaces
  e <- parseExpr
  return $ foldr Lam e x

parseExpr :: Parser Expr
parseExpr = do
  spaces
  p <- try parseLam <|> try parseApl <|> parseTerms 
  spaces
  return p
