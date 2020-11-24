{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
module TransIR where

import AST
import IR
import ProgState

import Control.Monad
import Control.Monad.State.Lazy

data ProgRecord = ProgRecord {
    globals :: [(VarName, Opd)],
    strings :: [(String, StrNum)],
    procs :: [ProcRecord] }
    deriving (Eq, Show)

data ProcRecord = ProcRecord {
    localsCount :: Int,
    tmpsCount :: Int,
    instrsIR :: [IRDirective],
    fnName :: FnName }
    deriving (Eq, Show)

data TransIR = TransIR {
    fnTypes :: [(FnName, RetType)],
    gbls :: [(VarName, Opd)],
    strs :: [(String, StrNum)],
    locals :: [(VarName, Opd)],
    tmpCounter :: Int,
    lblCounter :: Int,
    instrs :: [IRDirective],
    name :: FnName}
    deriving (Eq, Show)

initTransIR :: [(FnName, RetType)] 
                -> [(VarName, Opd)] 
                -> TransIR
initTransIR fns gs = TransIR {fnTypes = fns, 
                                gbls = gs,
                                strs = [],
                                locals = [],
                                tmpCounter = 0,
                                lblCounter = 0,
                                instrs = [],
                                name = ""}

newTransIR :: FnName -> TransIR -> TransIR
newTransIR id s = s {locals = [], 
                        tmpCounter = 0,
                        instrs = [],
                        name = id}

addStr :: String -> TransIR -> TransIR
addStr str s@(TransIR {strs = ss}) = s {strs = (str, length ss):ss}

addLocalOpd :: VarName -> DType -> TransIR -> TransIR
addLocalOpd id dt s@(TransIR {locals = ls}) = 
    let opd = LocalOpd dt (length ls)
    in s {locals = (id,opd):ls}

mkTmp :: DType -> State TransIR Opd
mkTmp dt = do
    i <- gets $ \s -> tmpCounter s
    modify $ \s -> s {tmpCounter = i + 1}
    return $ TmpOpd dt i

mkTmpFrom :: Opd -> State TransIR Opd
mkTmpFrom opd = mkTmp $ opdDType opd

incLabel :: State TransIR LabelNum
incLabel = do
    i <- gets $ \s -> lblCounter s
    modify $ \s -> s {lblCounter = i + 1}
    return $ i

addGlobalOpd :: VarName -> DType -> TransIR -> TransIR
addGlobalOpd id dt s@(TransIR {gbls = gs}) = s {gbls = (id, (GlobalOpd dt id)):gs}

addInstr :: IRDirective -> State TransIR ()
addInstr dir = modify $ appendInstr dir
    where
        appendInstr :: IRDirective -> TransIR -> TransIR
        appendInstr dir s@(TransIR {instrs = is}) = s {instrs = is ++ [dir]} 

addInstrs :: [IRDirective] -> State TransIR ()
addInstrs dirs = modify $ appendInstrs dirs
    where 
        appendInstrs :: [IRDirective] -> TransIR -> TransIR
        appendInstrs dirs s@(TransIR {instrs = is}) = s {instrs = is ++ dirs} 

addFormals :: Fn -> State TransIR ()
addFormals fn = do
    let formalIds = map fst $ fnFormals fn
    let opds = [LocalOpd dt i | (i,dt) <- zip [0..] (map snd $ fnFormals fn)]
    modify $ \s -> s {locals = (zip formalIds opds)}


-- Since we already performed name and type analysis,
-- we assume that this function will never fail to find
-- the target in the lookup
fromMaybe :: Maybe a -> a
fromMaybe (Just x) = x

getLocalOpd :: VarName -> TransIR -> Opd
getLocalOpd id s = fromMaybe $ lookup id $ locals s

getGlobalOpd :: VarName -> TransIR -> Opd
getGlobalOpd id s = fromMaybe $ lookup id $ gbls s

getOpd :: VarName -> TransIR -> Opd
getOpd id s = case lookup id (locals s) of
    Just x -> x
    Nothing -> getGlobalOpd id s

getStrNum :: String -> TransIR -> StrNum
getStrNum str s = fromMaybe $ lookup str $ strs s

getFnRetType :: FnName -> TransIR -> RetType
getFnRetType id s = fromMaybe $ lookup id $ fnTypes s