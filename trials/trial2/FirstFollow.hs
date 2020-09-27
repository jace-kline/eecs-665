module FirstFollow where

import Grammar
import Data.List
import Text.ParserCombinators.ReadP

type FirstSet = (Variable, [ProdElement])
type FollowSet = (Variable, [ProdElement])

saturate :: (Eq a) => (a -> a) -> a -> a
saturate f x = if x == f x then x else saturate f (f x)

initialSets :: Grammar -> [FirstSet]
initialSets g = map (\v -> (v, [])) $ vars g

lookupDerive :: Variable -> GrammarStruct -> [Prod]
lookupDerive v gs = fromMaybe $ lookup v gs

fromMaybe :: Maybe a -> a
fromMaybe (Just x) = x
fromMaybe _ = error "Tried to extract value from Nothing constructor"

removeOn :: (a -> Bool) -> [a] -> [a]
removeOn p xs = filter (\x -> not (p x)) xs

lookupSet :: Variable -> [FirstSet] -> [ProdElement]
lookupSet v sets = fromMaybe $ lookup v sets

addToSet :: FirstSet -> [ProdElement] -> FirstSet
addToSet (v,xs) xs' = (v, nub $ xs ++ xs')

updateSet :: [FirstSet] -> Variable -> [ProdElement] -> [FirstSet]
updateSet sets v xs = addToSet (v, lookupSet v sets) xs : removeOn (\(v',ps) -> v' == v) sets

firstSets :: Grammar -> [FirstSet]
firstSets g = saturate firstIter $ initialSets g
    where
        firstIter :: [FirstSet] -> [FirstSet]
        firstIter sets = map (firstVar sets) sets
        firstVar :: [FirstSet] -> FirstSet -> FirstSet
        firstVar sets set@(v,xs) = update sets set $ lookupDerive v $ grammar g
        update :: [FirstSet] -> FirstSet -> [Prod] -> FirstSet
        update sets set@(v,xs) prods = (v, nub $ foldr (++) [] $ map (updateOnProd sets set) prods)
        updateOnProd :: [FirstSet] -> FirstSet -> Prod -> [ProdElement]
        updateOnProd sets (v,xs) prod = (++) xs $ case prod of
            Epsilon -> [Eps]
            Prod ys -> f ys
            where
                f :: [ProdElement] -> [ProdElement]
                f [] = [Eps]
                f (z:zs) = case z of
                    Term z -> [Term z]
                    Var v  -> 
                        let ls = lookupSet v sets
                        in if Eps `elem` ls then (delete Eps ls) ++ (f zs) else ls
                    _      -> []


firstSet :: Grammar -> Variable -> [ProdElement]
firstSet g v = lookupSet v $ firstSets g

firstSetSequence :: Grammar -> [ProdElement] -> [ProdElement]
firstSetSequence g xs = case xs of
    [] -> [Eps]
    (y:ys) -> case y of
        Term t -> [Term t]
        Var v  -> 
            let s = firstSet g v
            in if Eps `elem` s then (delete Eps s) ++ (firstSetSequence g ys) else s
        _      -> []


varElementLists :: GrammarStruct -> [(Variable,[ProdElement])]
varElementLists g = concat $ map (\(v,ps) -> (zip (repeat v)) (map prodElements ps)) g

-- Associate the producing variable with the element sequence following the target variable in RHS productions
getFollowing :: Grammar -> Variable -> [(Variable, [ProdElement])]
getFollowing g v = map following $ removeOn (\(v',xs) -> not ((Var v) `elem` xs)) $ varElementLists (grammar g)
    where
        following :: (Variable, [ProdElement]) -> (Variable, [ProdElement])
        following (v',xs) = (v', tail $ dropWhile (\x -> x /= (Var v)) xs)

followSets :: Grammar -> [FollowSet]
followSets g = saturate followIter $ addEOFstart $ initialSets g
    where
        addEOFstart :: [FollowSet] -> [FollowSet]
        addEOFstart sets = updateSet sets (start g) [Eof]
        followIter :: [FollowSet] -> [FollowSet]
        followIter sets = map (followVar sets . fst) sets
        followVar :: [FollowSet] -> Variable -> FollowSet
        followVar sets v = (v, nub $ (lookupSet v sets) ++ (foldr (++) [] $ map (calcFollow sets) $ getFollowing g v))
        calcFollow :: [FollowSet] -> (Variable, [ProdElement]) -> [ProdElement]
        calcFollow sets (v,xs) = case xs of
            [] -> lookupSet v sets
            zs -> 
                let restFirstSet = firstSetSequence g zs
                in (delete Eps restFirstSet) ++ (if Eps `elem` restFirstSet then lookupSet v sets else [])

followSet :: Grammar -> Variable -> [ProdElement]
followSet g v = lookupSet v $ followSets g