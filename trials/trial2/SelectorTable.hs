module SelectorTable where

import Grammar
import FirstFollow

type Table r c v = [((r,c),v)]

type LL1Grammar = Grammar

type TableEntry = ((ProdElement, ProdElement), Prod)
type SelectorTable = Table ProdElement ProdElement Prod

selectorTable :: LL1Grammar -> SelectorTable
selectorTable g = addEOFstart : (foldr addToTable [] $ flattenGStruct $ grammar g)
    where
        addEOFstart = ((Var (start g), Eof), Epsilon) :: TableEntry
        addToTable :: (Variable, Prod) -> SelectorTable -> SelectorTable
        addToTable (v,prod) t = t ++ (concat $ map toEntries first)
            where
                first = firstSetSequence g $ prodElements prod :: [ProdElement]
                follow = followSet g v :: [ProdElement]
                toEntries :: ProdElement -> [TableEntry]
                toEntries x = case x of
                    Eps    -> concat $ map toEntries follow
                    _      -> [((Var v, x), prod)]

mkSelectorTable :: String -> Either ErrMsg SelectorTable
mkSelectorTable input = mkLL1Grammar input >>= \g -> return $ selectorTable g
