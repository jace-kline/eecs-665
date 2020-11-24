{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
module ToMIPS where

import MIPS
import IR
import TransIR
import AST

import Control.Monad
import Control.Monad.Writer.Lazy

tell' :: b -> Writer [b] ()
tell' x = tell [x]

toMIPS :: ProgRecord -> [MIPSLine]
toMIPS prog = execWriter (toMIPSWriter prog)

toMIPSWriter:: ProgRecord -> Writer [MIPSLine] ()
toMIPSWriter prog = do
    tell' DataSection
    tell [GlobalDecl id | (id, _) <- globals prog]
    tell [StringDecl i str | (str, i) <- strings prog]
    tell' TextSection
    tell' DotGloblMain
    sequence_ $ map procToMIPS (procs prog)
    
procToMIPS :: ProcRecord -> Writer [MIPSLine] ()
procToMIPS p = do
    let moveSP = wordSize * (localsCount p + tmpsCount p)
    tell' $ LabelLine (fnName p)
    tell' $ load R_t2 (ImmOpd (moveSP + wordSize))
    tell' $ BinOpInstr SUB' R_sp R_sp R_t2
    tell' $ store R_ra (OffsetOpd R_sp moveSP)
    sequence_ $ map (irToMips p) (instrsIR p)
    tell' $ LabelLine (leaveLabel $ fnName p)
    tell' $ load R_ra (OffsetOpd R_sp moveSP)
    tell' $ load R_t2 (ImmOpd (moveSP + wordSize))
    tell' $ BinOpInstr ADD' R_sp R_sp R_t2
    if fnName p == "main"
        then do
            tell' $ load R_v0 (ImmOpd 10)
            tell' $ Syscall
        else tell' $ Jr R_ra

irToMips :: ProcRecord -> IRDirective -> Writer [MIPSLine] ()
irToMips p ir = case ir of
    Assign l r -> do
        tell' $ load R_t0 (opdToMips r)
        tell' $ store R_t0 (opdToMips l)
    BinOpDirective op dst l r -> do
        tell' $ load R_t0 (opdToMips l)
        tell' $ load R_t1 (opdToMips r)
        tell' $ BinOpInstr op R_t0 R_t0 R_t1
        tell' $ store R_t0 (opdToMips dst)
    LabelNop i -> tell' $ LabelLine (labelNumLabel i)
    Jump i -> tell' $ J (labelNumLabel i)
    JumpLeave id -> tell' $ J (leaveLabel $ fnName p)
    IfZero opd i -> do
        tell' $ load R_t0 (opdToMips opd)
        tell' $ Bne0 R_t0 (labelNumLabel i)
    GetRet opd -> tell' $ store R_v0 (opdToMips opd)
    SetRet opd -> tell' $ load R_v0 (opdToMips opd)
    Enter _ -> return ()
    Leave _ -> return ()
    GetArg i opd -> tell' $ store (argReg i) (opdToMips opd)
    SetArg i opd -> tell' $ load (argReg i) (opdToMips opd)
    Call id -> tell' $ Jal (fnLabel id)
    GetInput opd -> do
        let syscallspec = case opdDType opd of
                IntT -> 5
                BoolT -> 5
                CharT -> 12
                CharPtrT -> error "This implementation does not support FROMCONSOLE with string arguments"
        tell' $ load R_a0 (ImmOpd syscallspec)
        tell' $ Syscall
        tell' $ store R_v0 (opdToMips opd)
    Output opd -> do
        let syscallspec = case opdDType opd of
                IntT -> 1
                BoolT -> 1
                CharT -> 11
                CharPtrT -> 4
        tell' $ load R_a0 (opdToMips opd)
        tell' $ load R_v0 (ImmOpd syscallspec)
        tell' $ Syscall
    where
        opdToMips :: Opd -> MIPSOpd
        opdToMips opd = case opd of
            (LitOpd t i) -> ImmOpd i
            (LocalOpd t i) -> OffsetOpd R_sp (localOffset i)
            (GlobalOpd t id) -> LblOpd (gblLabel id)
            (StrRefOpd i) -> AddrLblOpd (strLabel i)
            (TmpOpd t i) -> OffsetOpd R_sp (tmpOffset i)
        localOffset :: LocalNum -> Offset
        localOffset i = wordSize * i
        tmpOffset :: TmpNum -> Offset
        tmpOffset i = (wordSize * (localsCount p)) + (wordSize * i)