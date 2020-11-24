{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
module ToIR where

import AST
import IR
import TransIR
import ProgState
import Analysis (succeed)

import Control.Monad
import Control.Monad.State.Lazy
import Data.Char (ord)

progIR :: [(VarName, DType)] -> [(FnName, Fn)] -> ProgRecord
progIR gbltypes fns = ProgRecord { globals = gbls s, strings = strs s, procs = proc_recs}
    where
        gblopds = zip (map fst gbltypes) (map gblToOpd gbltypes)
        fnrets = zip (map fst fns) (map (fnRetType . snd) fns)
        (proc_recs, s) = runState (fnsToIR fns) (initTransIR fnrets gblopds)
        gblToOpd :: (Id, DType) -> Opd
        gblToOpd (id, dt) = GlobalOpd dt id

fnsToIR :: [(FnName, Fn)] -> State TransIR [ProcRecord]
fnsToIR fns = mapM (uncurry fnToIR) fns

fnToIR :: FnName -> Fn -> State TransIR ProcRecord
fnToIR id fn = do
    modify $ newTransIR id
    addInstr $ Enter id
    addFormals fn
    formals <- gets locals
    addInstrs [GetArg i opd | (i, opd) <- zip [0..] (map snd formals)]
    effsToIR $ fnEffects fn
    addInstr $ Leave id
    s <- get
    return (ProcRecord {localsCount = length (locals s),
                        tmpsCount = tmpCounter s,
                        instrsIR = instrs s,
                        fnName = id})
    


effsToIR :: [Effect] -> State TransIR ()
effsToIR effs = sequence_ $ map effToIR effs


effToIR :: Effect -> State TransIR ()
effToIR (VarDecl id dt) = modify (addLocalOpd id dt) >> succeed
effToIR (AssignStmt e) = expToIR e >> succeed
effToIR (Dec id) = effToIR (AssignStmt (id := ((ID id) :-: (IntLit 1))))
effToIR (Inc id) = effToIR (AssignStmt (id := ((ID id) :+: (IntLit 1))))
effToIR (FromConsole id) = do
    opd <- gets $ getOpd id
    addInstr $ GetInput opd
effToIR (ToConsole e) = do
    opd <- expToIR e
    addInstr $ Output opd
effToIR (If c effs) = do
    l <- incLabel
    c_opd <- expToIR c
    addInstr $ IfZero c_opd l
    effsToIR effs
    addInstr $ LabelNop l
effToIR (IfElse c effs1 effs2) = do
    l <- incLabel
    l' <- incLabel
    c_opd <- expToIR c
    addInstr $ IfZero c_opd l
    effsToIR effs1
    addInstr $ Jump l'
    addInstr $ LabelNop l
    effsToIR effs2
    addInstr $ LabelNop l'
effToIR (While c effs) = do
    l <- incLabel
    l' <- incLabel
    c_opd <- expToIR c
    addInstr $ LabelNop l
    addInstr $ IfZero c_opd l'
    effsToIR effs
    addInstr $ Jump l
    addInstr $ LabelNop l'
effToIR (Return me) = do
    fn <- gets name
    case me of
        Nothing -> addInstr $ JumpLeave fn
        Just e  -> do
            opd <- expToIR e
            addInstrs [SetRet opd, JumpLeave fn]
effToIR (FnCallStmt e@(FnCall id args)) = do
    t <- gets $ getFnRetType id
    case t of
        VoidT -> succeed
        _     -> expToIR e >> succeed
effToIR _ = error "Bad eff encountered"


expToIR :: Exp -> State TransIR Opd
expToIR (l := r) = do
    l' <- gets $ getOpd l
    r' <- expToIR r
    addInstr $ Assign l' r'
    return l'
expToIR (l :||: r) = binIR l r OR' BoolT
expToIR (l :&&: r) = binIR l r AND' BoolT
expToIR (l :>: r) = binIR r l LT' BoolT
expToIR (l :<: r) = binIR l r LT' BoolT
expToIR (l :>=: r) = expToIR ((l :>: r) :||: (l :==: r))
expToIR (l :<=: r) = expToIR ((l :<: r) :||: (l :==: r))
expToIR (l :==: r) = expToIR (Not (l :!=: r))
expToIR (l :!=: r) = expToIR ((l :<: r) :||: (r :<: l))
expToIR (l :+: r) = binIR l r ADD' IntT
expToIR (l :-: r) = binIR l r SUB' IntT
expToIR (l :*: r) = binIR l r MUL' IntT
expToIR (l :/: r) = binIR l r DIV' IntT
expToIR (Neg e) = expToIR ((IntLit (-1)) :*: e)
expToIR (Not e) = fmap (\x -> cast x BoolT) $ expToIR ((IntLit 1) :-: e)
expToIR (IntLit i) = return $ LitOpd IntT i
expToIR (CharLit c) = return $ LitOpd CharT (ord c)
expToIR (BoolLit b) = return $ LitOpd BoolT (if b == False then 0 else 1)
expToIR (StrLit s) = do
    modify $ addStr s
    strnum <- gets $ getStrNum s
    return $ StrRefOpd strnum
expToIR VoidLit = error "Expression is void"
expToIR (FnCall id args) = do
    opds <- mapM expToIR args
    addInstrs [SetArg i opd | (i, opd) <- zip [0..] opds]
    addInstr $ Call id
    rt <- gets $ getFnRetType id
    case rt of
        VoidT -> error "Fn call expression with void return"
        _     -> do
            dst <- mkTmp rt
            addInstr $ GetRet dst
            return dst
expToIR (ID id) = gets $ getOpd id

binIR :: Exp -> Exp -> BinOp -> DType -> State TransIR Opd
binIR l r op dt = do
    l' <- expToIR l
    r' <- expToIR r
    dst <- mkTmp dt
    addInstr $ BinOpDirective op dst l' r'
    return dst
