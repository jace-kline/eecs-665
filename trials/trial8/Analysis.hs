{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
module Analysis where
-- Name and Type Analysis

import AST
import ProgState

import Control.Monad
import Control.Monad.Identity
import Control.Monad.State.Lazy
import Control.Monad.Trans

type ProcessM s v m = StateT s (MaybeT m) v
type AnalyzeM s v = ProcessM s v Identity
type ActuateM s v = ProcessM s v IO

succeed :: Monad m => m ()
succeed = return ()

effectAnalyzer :: Effect -> AnalyzeM Scope ()
effectAnalyzer f@(FnDecl id _ _ _) = do
    Nothing <- gets $ getFn id
    modify $ putFn $ mkFn f
    succeed
-- effectAnalyzer (VarDecl id t) = do
--     No
--     modify (putType (id,t)) >> succeed
-- effectAnalyzer (Assign)