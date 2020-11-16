{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
module Eval where
-- fold over the desired abstract syntax to produce
-- a change in program state

import AST
import ProgState
import Analysis

import Control.Monad
import Control.Monad.Trans.Maybe
import Control.Monad.Identity
import Control.Monad.State.Lazy
import Control.Monad.Trans
import Text.Read (readMaybe)

type InterpM v = StateT Scope (MaybeT IO) v

runInterpM :: Scope -> InterpM a -> IO (Maybe Scope)
runInterpM s m = runMaybeT $ execStateT m s


-- only change the state if all actions are successful
effsRun :: [Effect] -> InterpM Bool
effsRun effs = do
    s <- get
    ms <- tryEffs s
    case ms of
        Nothing -> return False
        Just s' -> put s' >> return True
    where
        tryEffs :: Scope -> InterpM (Maybe Scope)
        tryEffs s = do
            let res = runInterpM s (sequence_ (map effRun effs))
            maybescope <- liftIO res
            return maybescope

effRun :: Effect -> InterpM ()
effRun eff = do
    s <- get
    if haveReturned s
        then succeed
        else effRun' eff

effRun' :: Effect -> InterpM ()
effRun' (AssignStmt e) = expRun e >> succeed
effRun' (Dec id) = effRun' (AssignStmt (id := ((ID id) :-: (IntLit 1))))
effRun' (Inc id) = effRun' (AssignStmt (id := ((ID id) :+: (IntLit 1))))
effRun' (FromConsole id) = do
    t <- getTypeM id
    s <- liftIO $ do
        putStr "> "
        getLine
    let mint = readMaybe s :: Maybe Int
    let mchar = readMaybe s :: Maybe Char
    case t of
        IntT -> case mint of
            Nothing -> mzero
            Just i -> putValM id (IntLit i)
        BoolT -> case mint of
            Nothing -> mzero
            Just i -> if i == 0
                        then putValM id (BoolLit False)
                        else if i == 1
                            then putValM id (BoolLit True)
                            else mzero
        CharT -> case mchar of
            Nothing -> mzero
            Just c -> putValM id (CharLit c)
        CharPtrT -> putValM id (StrLit s)
effRun' (ToConsole e) = do
    v <- expRun e
    case v of
        (IntLit i) -> liftIO $ putStr $ show i
        (BoolLit b) -> liftIO $ putStr $ show b
        (CharLit c) -> liftIO $ putStr [c]
        (StrLit s) -> liftIO $ putStr s
        _ -> mzero
effRun' (If cond effs) = do
    (BoolLit c) <- expRun cond
    if c then effsRun effs >> succeed else succeed
effRun' (IfElse cond effs1 effs2) = do
    (BoolLit c) <- expRun cond
    effsRun (if c then effs1 else effs2) >> succeed
effRun' w@(While cond effs) = do
    (BoolLit c) <- expRun cond
    if c 
        then effsRun effs >> effRun w 
        else succeed
effRun' eff@(Return me) = (case me of
        Nothing -> putRetM Nothing
        Just e  -> putRetValM e) >> (modify setHaveReturned)
effRun' (FnCallStmt e) = expRun e >> succeed
effRun' eff = effAnalysis eff


expRun :: Exp -> InterpM Exp
expRun (id := e) = expRun e >>= \v -> putValM id v >> return v
expRun (l :||: r) = binBool l r (||)
expRun (l :&&: r) = binBool l r (&&)
expRun (l :>: r) = binCmp l r (>)
expRun (l :>=: r) = binCmp l r (>=)
expRun (l :<: r) = binCmp l r (<)
expRun (l :<=: r) = binCmp l r (<=)
expRun (l :==: r) = eqNeqCmp l r (==)
expRun (l :!=: r) = eqNeqCmp l r (/=)
expRun (l :+: r) = binArith l r (+)
expRun (l :-: r) = binArith l r (-)
expRun (l :*: r) = binArith l r (*)
expRun (l :/: r) = binArith l r div
expRun (Neg e) = expRun (e :*: (IntLit (-1)))
expRun (Not e) = expRun e >>= \(BoolLit b) -> return $ BoolLit (not b)
expRun (FnCall id argexps) = do
    fn <- getFnM id
    let paramIds = map fst $ fnFormals fn
    let effs = fnEffects fn
    argvs <- mapM expRun argexps
    enterScope fn
    sequence_ [putValM i v | (i, v) <- zip paramIds argvs]
    effsRun effs
    rv <- getRetValM
    exitScope
    return rv
expRun (ID id) = getValM id
expRun lit = return lit

binBool :: Exp -> Exp -> (Bool -> Bool -> Bool) -> InterpM Exp
binBool l r f = do
    (BoolLit vl) <- expRun l
    (BoolLit vr) <- expRun r
    return $ BoolLit (f vl vr)

binCmp :: Exp -> Exp -> (Int -> Int -> Bool) -> InterpM Exp
binCmp l r f = do
    (IntLit vl) <- expRun l
    (IntLit vr) <- expRun r
    return $ BoolLit (f vl vr)

eqNeqCmp :: Exp -> Exp -> (Exp -> Exp -> Bool) -> InterpM Exp
eqNeqCmp l r f = do
    vl <- expRun l
    vr <- expRun r
    return $ BoolLit (f vl vr)

binArith :: Exp -> Exp -> (Int -> Int -> Int) -> InterpM Exp
binArith l r f = do
    (IntLit vl) <- expRun l
    (IntLit vr) <- expRun r
    return $ IntLit (f vl vr)