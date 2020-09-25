module Grammar where

import Data.Char
import Data.List
import Text.ParserCombinators.ReadP
import Parser

type Variable = String
type Terminal = String
type ErrMsg = String
-- type GrammarStruct = [(Variable, [Prod])]
type GrammarStruct = [VarDerive]
type VarDerive = (Variable, [Prod])

prodElemLists :: [Prod] -> [[ProdElement]]
prodElemLists prods = filter (\xs -> not (null xs)) $ map prodElements prods

data Prod = Prod [ProdElement]
          | Epsilon
          deriving (Eq,Show)

prodElements :: Prod -> [ProdElement]
prodElements Epsilon = []
prodElements (Prod ls) = ls

data ProdElement = Var Variable
                 | Term Terminal
                 | Eps
                 deriving (Eq,Show)

unProdElem :: ProdElement -> String
unProdElem (Var v) = v
unProdElem (Term t) = t
unProdElem Eps = ""

data LineLex = Assign String [String]
             | Bar [String]
             deriving (Eq,Show)

data Grammar = Grammar { start :: Variable, vars :: [Variable], terms :: [Terminal], grammar :: GrammarStruct }
    deriving (Show)

maybeToEither :: e -> Maybe a -> Either e a
maybeToEither e ma = case ma of
    Nothing  -> Left e
    (Just x) -> Right x

mkLL1Grammar :: String -> Either ErrMsg Grammar
mkLL1Grammar input = do
    g <- maybeToEither "Error in grammar spec: Parse error" $ mkGrammar input
    let g_struct = grammar g
    if leftRecursive g_struct
        then Left "Error in grammar spec: Immediate left recursion detected"
        else if not (leftFactored g_struct)
                then Left "Error in grammar spec: The grammar is not left factored"
                else Right g 

mkGrammar :: String -> Maybe Grammar
mkGrammar input = do
    ls <- parseGrammarLines input
    let (s, vs, ts) = (getStart ls, getVars ls, getTerms ls)
    g <- varDerivations ls
    return $ Grammar { start = s, vars = vs, terms = ts, grammar = g }

isLL1 :: GrammarStruct -> Bool
isLL1 g = not (leftRecursive g) && leftFactored g

leftRecursive :: GrammarStruct -> Bool
leftRecursive xs = any id $ map g xs
    where 
        g :: VarDerive -> Bool
        g (v,prods) = any id $ map (q v) prods
        q :: Variable -> Prod -> Bool
        q v Epsilon = False
        q v (Prod (y:ys)) = case y of
            Var v' -> v == v'
            _      -> False
        q v _ = True

leftFactored :: GrammarStruct -> Bool
leftFactored xs = all id $ map (g . prodElemLists . snd) xs
    where
        g :: [[ProdElement]] -> Bool
        g lls = (length lls) == (length $ groupBy (==) $ sort $ map (unProdElem . head) lls)


parseGrammarLines :: String -> Maybe [LineLex]
parseGrammarLines s = sequence $ map ((\xs -> if null xs then Nothing else Just (extract xs)) . (runParser lineParser)) (lines s)

getVars :: [LineLex] -> [Variable]
getVars = nub . foldr (\x r -> case x of {(Assign v _) -> (v:r); _ -> r}) []

getTerms :: [LineLex] -> [Terminal]
getTerms ls =
    let vars = getVars ls
    in nub $ filter (\s -> not (s `elem` vars)) $ concat $ map unLex ls
    where
        unLex :: LineLex -> [String]
        unLex (Assign _ ys) = ys
        unLex (Bar ys) = ys

getStart :: [LineLex] -> Variable
getStart = head . getVars

varDerivations :: [LineLex] -> Maybe GrammarStruct
varDerivations xs = (f (reverse xs)) >>= \rs -> return $ combineSameVars $ reverse rs
    where
        f :: [LineLex] -> Maybe GrammarStruct
        f [] = return []
        f [x] = case x of
            Bar _ -> Nothing
            Assign v ys -> return $ [(v, [toProd ys])]
        f (x:y:xs) = do
            ((v, prods):zs) <- f (y:xs)
            case x of
                (Bar as) -> return $ (v, prods ++ [toProd as]) : zs
                (Assign w as) -> return $ (w, [toProd as]) : ((v,prods):zs)
        toProd :: [String] -> Prod
        toProd ys = case map toProdElem ys of
            [] -> Epsilon
            xs -> Prod xs
        toProdElem :: String -> ProdElement
        toProdElem s = 
            let vars = getVars xs
            in if s `elem` vars then Var s else Term s
        combineSameVars :: GrammarStruct -> GrammarStruct
        combineSameVars xs = map g $ groupBy (\x y -> fst x == fst y) $ sortOn fst xs
            where g ys = (fst (head ys), concat $ map snd ys)

lineParser :: ReadP LineLex
lineParser = assignParser +++ barParser

assignParser :: ReadP LineLex
assignParser = do
    lhs <- symbolParser
    string "::="
    rhs <- many $ symbolParser
    return $ Assign lhs rhs

symbolParser :: ReadP String
symbolParser = trimSpaces $ many1 (satisfy isAlphaNum)

barParser :: ReadP LineLex
barParser = trimSpaces $ do
    char '|'
    xs <- many $ symbolParser
    return $ Bar xs