{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
module MIPS where

import IR

type Bytes = Int
type Offset = Int
type Label = String

wordSize :: Bytes
wordSize = 4

data MIPSOpd where
    LblOpd :: Label -> MIPSOpd
    AddrLblOpd :: Label -> MIPSOpd
    ImmOpd :: Int -> MIPSOpd
    OffsetOpd :: Reg -> Offset -> MIPSOpd
    deriving (Eq)

instance Show MIPSOpd where
    show opd = case opd of
        LblOpd lbl -> lbl
        AddrLblOpd lbl -> lbl
        ImmOpd i -> show i
        OffsetOpd reg off -> show off ++ "(" ++ show reg ++ ")"

load :: Reg -> MIPSOpd -> MIPSLine
load reg opd@(LblOpd l) = Lw reg opd
load reg opd@(AddrLblOpd l) = La reg opd
load reg opd@(ImmOpd i) = Li reg opd
load reg opd@(OffsetOpd r o) = Lw reg opd

store :: Reg -> MIPSOpd -> MIPSLine
store reg opd@(LblOpd l) = Sw reg opd
store reg opd@(OffsetOpd r o) = Sw reg opd

data Reg = R_zero 
         | R_v0 
         | R_a0 | R_a1 | R_a2 | R_a3
         | R_t0 | R_t1 | R_t2
         | R_sp
         | R_ra
    deriving (Eq)

instance Show Reg where
    show reg = case reg of
        R_zero -> "$0"
        R_v0 -> "$v0"
        R_a0 -> "$a0"
        R_a1 -> "$a1"
        R_a2 -> "$a2"
        R_a3 -> "$a3"
        R_t0 -> "$t0"
        R_t1 -> "$t1"
        R_t2 -> "$t2"
        R_sp -> "$sp"
        R_ra -> "$ra"

argReg :: ArgNum -> Reg
argReg 0 = R_a0
argReg 1 = R_a1
argReg 2 = R_a2
argReg 3 = R_a3

data MIPSLine where
    DataSection :: MIPSLine
    TextSection :: MIPSLine
    DotGloblMain :: MIPSLine
    GlobalDecl :: Label -> MIPSLine
    StringDecl :: StrNum -> String -> MIPSLine
    LabelLine :: Label -> MIPSLine
    Lw :: Reg -> MIPSOpd -> MIPSLine
    Li :: Reg -> MIPSOpd -> MIPSLine
    La :: Reg -> MIPSOpd -> MIPSLine
    Sw :: Reg -> MIPSOpd -> MIPSLine
    Bne0 :: Reg -> Label -> MIPSLine
    J :: Label -> MIPSLine
    Jr :: Reg -> MIPSLine
    Jal :: Label -> MIPSLine
    BinOpInstr :: BinOp -> Reg -> Reg -> Reg -> MIPSLine
    Syscall :: MIPSLine
    deriving (Eq)

instance Show MIPSLine where
    show x = case x of
        DataSection -> ".data"
        TextSection -> ".text"
        DotGloblMain -> ".globl main"
        GlobalDecl lbl -> lbl ++ ": .word 0"
        StringDecl i str -> strLabel i ++ ": .asciiz " ++ show str
        LabelLine lbl -> lbl ++ ":"
        Lw reg opd -> "lw " ++ show reg ++ ", " ++ show opd
        Li reg opd -> "li " ++ show reg ++ ", " ++ show opd
        La reg opd -> "la " ++ show reg ++ ", " ++ show opd
        Sw reg opd -> "sw " ++ show reg ++ ", " ++ show opd
        Bne0 reg lbl -> "bne $0, " ++ show reg ++ ", " ++ lbl
        J lbl -> "j " ++ lbl
        Jr reg -> "jr " ++ show reg
        Jal reg -> "jal " ++ show reg
        BinOpInstr op dst l r -> reprOp op ++ " " ++ show dst ++ ", " ++ show l ++ ", " ++ show r
        Syscall -> "syscall"

fnLabel :: FnName -> Label
fnLabel id = id

strLabel :: StrNum -> Label
strLabel i = "str_" ++ show i

gblLabel :: VarName -> Label
gblLabel id = id

leaveLabel :: FnName -> Label
leaveLabel id = "leave_" ++ id

labelNumLabel :: LabelNum -> Label
labelNumLabel i = "lbl_" ++ show i