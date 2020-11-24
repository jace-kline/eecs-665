{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
module IR where

import AST

type FnName = String
type VarName = String

type LocalNum = Int
type StrNum = Int
type LabelNum = Int
type ArgNum = Int
type TmpNum = Int

data Opd where
    LitOpd :: DType -> Int -> Opd
    LocalOpd :: DType -> LocalNum -> Opd
    GlobalOpd :: DType -> VarName -> Opd
    StrRefOpd :: StrNum -> Opd
    TmpOpd :: DType -> TmpNum -> Opd
    deriving (Eq, Show)

opdDType :: Opd -> DType
opdDType (LitOpd t _) = t
opdDType (LocalOpd t _) = t
opdDType (GlobalOpd t _) = t
opdDType (StrRefOpd _) = CharPtrT
opdDType (TmpOpd t _) = t

cast :: Opd -> DType -> Opd
cast (LitOpd t x) t' = LitOpd t' x
cast (LocalOpd t x) t' = LocalOpd t' x
cast (GlobalOpd t x) t' = GlobalOpd t' x
cast (TmpOpd t x) t' = TmpOpd t' x
cast _ _ = error "Invalid cast"

data IRDirective where
    Assign :: Opd -> Opd -> IRDirective
    BinOpDirective :: BinOp -> Opd -> Opd -> Opd -> IRDirective
    LabelNop :: LabelNum -> IRDirective
    Jump :: LabelNum -> IRDirective
    JumpLeave :: FnName -> IRDirective
    IfZero :: Opd -> LabelNum -> IRDirective
    GetRet :: Opd -> IRDirective
    SetRet :: Opd -> IRDirective
    Enter :: FnName -> IRDirective
    Leave :: FnName -> IRDirective
    GetArg :: ArgNum -> Opd -> IRDirective
    SetArg :: ArgNum -> Opd -> IRDirective
    Call :: FnName -> IRDirective
    GetInput :: Opd -> IRDirective
    Output :: Opd -> IRDirective
    deriving (Eq, Show)

data BinOp = OR' | AND' | LT' | ADD' | SUB' | MUL' | DIV'
    deriving (Eq, Show)

reprOp :: BinOp -> String
reprOp op = case op of
    OR' -> "or"
    AND' -> "and"
    LT' -> "slt"
    ADD' -> "add"
    SUB' -> "sub"
    MUL' -> "mul"
    DIV' -> "div"