{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
module Analysis where
-- Name and Type Analysis

import AST
import ProgState

import Control.Applicative
import Control.Monad
import Control.Monad.Trans.Maybe
import Control.Monad.Identity
import Control.Monad.State.Lazy
import Control.Monad.Trans

type ProcessM m v = StateT Scope (MaybeT m) v

coerceFromMaybe :: (MonadPlus m) => Maybe a -> m a
coerceFromMaybe = maybe mzero return

coerceToMaybe :: MaybeT Identity a -> Maybe a
coerceToMaybe = runIdentity . runMaybeT

liftMaybe :: (Monad m) => Maybe a -> StateT s (MaybeT m) a
liftMaybe = lift . coerceFromMaybe

analyzer :: ProcessM Identity () -> Scope -> Maybe Scope
analyzer p s = coerceToMaybe $ execStateT p s

effsAnalyzer :: [Effect] -> Scope -> Maybe Scope
effsAnalyzer effs s = coerceToMaybe $ execStateT (effsAnalysis effs) s

effAnalyzer :: Effect -> Scope -> Maybe Scope
effAnalyzer eff s = coerceToMaybe $ execStateT (effAnalysis eff) s

stateLift :: Monad m => (s -> Maybe a) -> StateT s (MaybeT m) a
stateLift f = get >>= \s -> liftMaybe (f s)

getFnM :: Monad m => Id -> ProcessM m Fn
getFnM id = stateLift $ getFn id

getTypeM :: Monad m => Id -> ProcessM m DType
getTypeM id = stateLift $ getType id

getValM :: Monad m => Id -> ProcessM m Exp
getValM id = stateLift $ getVal id

getRetValM :: Monad m => ProcessM m Exp
getRetValM = get >>= return . retVal

putTypeM :: Monad m => Id -> DType -> ProcessM m ()
putTypeM id t = modify $ putType id t

putValM :: Monad m => Id -> Exp -> ProcessM m ()
putValM id t = modify $ putVal id t

putRetValM :: Monad m => Exp -> ProcessM m ()
putRetValM e = modify $ putRet e

enterScope :: Monad m => Fn -> ProcessM m ()
enterScope fn = modify $ newScope fn

exitScope :: Monad m => ProcessM m ()
exitScope = stateLift popScope >>= put

succeed :: Monad m => m ()
succeed = return ()

effsAnalysis :: Monad m => [Effect] -> ProcessM m ()
effsAnalysis effs = sequence_ $ map effAnalysis effs


effAnalysis :: Monad m => Effect -> ProcessM m ()
effAnalysis f@(FnDecl id rt formals effs) = do
    Nothing <- gets $ getFn id
    let fn = mkFn f
    modify $ putFn id fn
    enterScope fn
    effsAnalysis effs
    exitScope
effAnalysis (VarDecl id t) = do
    s <- get
    if isGblScope s 
        then case getGblType id s of
                Just _ -> mzero
                Nothing -> putTypeM id t
        else case getLocalType id s of
                Just _ -> mzero
                Nothing -> putTypeM id t
effAnalysis (AssignStmt e) = expAnalysis e >> succeed
effAnalysis (Dec id) = incDec id
effAnalysis (Inc id) = incDec id
effAnalysis (FromConsole id) = exists id
effAnalysis (ToConsole e) = expAnalysis e >> succeed
effAnalysis (If e effs) = branchAnalysis e effs
effAnalysis (IfElse e effs effs') = do
    branchAnalysis e effs
    effsAnalysis effs'
effAnalysis (While e effs) = branchAnalysis e effs
effAnalysis (Return mexp) = do
    s <- get
    case mexp of
        Nothing -> case retType s of
            VoidT -> succeed
            _ -> mzero
        Just e -> do
            t <- expAnalysis e
            guard (t == retType s)
            succeed
effAnalysis (FnCallStmt e) = expAnalysis e >> succeed

expAnalysis :: Monad m => Exp -> ProcessM m DType
expAnalysis (id := e) = do
    tl <- getTypeM id
    tr <- expAnalysis e
    guard (tl == tr)
    return tl
expAnalysis (l :||: r) = binOpAnalysis l r BoolT BoolT BoolT
expAnalysis (l :&&: r) = binOpAnalysis l r BoolT BoolT BoolT
expAnalysis (l :>: r) = binOpAnalysis l r IntT IntT BoolT
expAnalysis (l :>=: r) = binOpAnalysis l r IntT IntT BoolT
expAnalysis (l :<: r) = binOpAnalysis l r IntT IntT BoolT
expAnalysis (l :<=: r) = binOpAnalysis l r IntT IntT BoolT
expAnalysis (l :==: r) = eqNeq l r
expAnalysis (l :!=: r) = eqNeq l r
expAnalysis (l :+: r) = binOpAnalysis l r IntT IntT IntT
expAnalysis (l :-: r) = binOpAnalysis l r IntT IntT IntT
expAnalysis (l :*: r) = binOpAnalysis l r IntT IntT IntT
expAnalysis (l :/: r) = binOpAnalysis l r IntT IntT IntT
expAnalysis (Neg e) = binOpAnalysis (IntLit 1) e IntT IntT IntT
expAnalysis (Not e) = binOpAnalysis (BoolLit True) e BoolT BoolT BoolT
expAnalysis (IntLit _) = return IntT
expAnalysis (CharLit _) = return CharT
expAnalysis (BoolLit _) = return BoolT
expAnalysis (StrLit _) = return CharPtrT
expAnalysis VoidLit = return VoidT
expAnalysis (FnCall id args) = do
    (Just fn) <- gets $ getFn id
    let formalts = map snd $ fnFormals fn
    actualts <- mapM expAnalysis args
    guard (actualts == formalts)
    return $ fnRetType fn
expAnalysis (ID id) = getTypeM id

binOpAnalysis :: Monad m => Exp -> Exp -> DType -> DType -> RetType -> ProcessM m DType
binOpAnalysis el er tl tr rt = do
    tl' <- expAnalysis el
    guard (tl' == tl)
    tr' <- expAnalysis er
    guard (tr' == tr)
    return rt

eqNeq :: Monad m => Exp -> Exp -> ProcessM m DType
eqNeq el er = do
    tl <- expAnalysis el
    tr <- expAnalysis er
    guard (tl == tr)
    return BoolT

incDec :: Monad m => Id -> ProcessM m ()
incDec id = do
    IntT <- getTypeM id
    succeed

exists :: Monad m => Id -> ProcessM m ()
exists id = do
    getTypeM id
    succeed

branchAnalysis :: Monad m => Exp -> [Effect] -> ProcessM m ()
branchAnalysis e effs = do
    BoolT <- expAnalysis e
    effsAnalysis effs