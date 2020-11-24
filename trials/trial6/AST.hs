{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
module AST where
-- define abstract syntax for HoleyC language structures

type Id = String
type RetType = DType
type ArgType = DType


data DType where
    VoidT :: DType
    IntT :: DType
    BoolT :: DType
    CharT :: DType
    CharPtrT :: DType
    deriving (Eq, Show)

{-
An 'Effect' is a declaration or a statement.
It changes the program state / interacts with OS by:
    - updating internal variable, type stores
    - input/output with user
-}
data Effect where
    VarDecl :: Id -> DType -> Effect
    FnDecl :: Id -> RetType -> [(Id, ArgType)] -> [Effect] -> Effect
    AssignStmt :: Exp -> Effect
    Dec :: Id -> Effect
    Inc :: Id -> Effect
    FromConsole :: Id -> Effect
    ToConsole :: Exp -> Effect
    If :: Exp -> [Effect] -> Effect
    IfElse :: Exp -> [Effect] -> [Effect] -> Effect
    While :: Exp -> [Effect] -> Effect
    Return :: Maybe Exp -> Effect
    FnCallStmt :: Exp -> Effect
    deriving (Eq, Show)

infixr 1 :=
infixr 2 :||:
infixr 3 :&&:
infix 4 :>:
infix 4 :>=:
infix 4 :<:
infix 4 :<=:
infix 4 :==:
infix 4 :!=:
infixl 6 :+:
infixl 6 :-:
infixl 7 :*:
infixl 7 :/:

data Exp where
    (:=) :: Id -> Exp -> Exp
    (:||:) :: Exp -> Exp -> Exp
    (:&&:) :: Exp -> Exp -> Exp
    (:>:) :: Exp -> Exp -> Exp
    (:>=:) :: Exp -> Exp -> Exp
    (:<:) :: Exp -> Exp -> Exp
    (:<=:) :: Exp -> Exp -> Exp
    (:==:) :: Exp -> Exp -> Exp
    (:!=:) :: Exp -> Exp -> Exp
    (:+:) :: Exp -> Exp -> Exp
    (:-:) :: Exp -> Exp -> Exp
    (:*:) :: Exp -> Exp -> Exp
    (:/:) :: Exp -> Exp -> Exp
    Neg :: Exp -> Exp
    Not :: Exp -> Exp
    IntLit :: Int -> Exp
    CharLit :: Char -> Exp
    BoolLit :: Bool -> Exp
    StrLit :: String -> Exp
    VoidLit :: Exp
    FnCall :: Id -> [Exp] -> Exp
    ID :: Id -> Exp
    deriving (Eq, Show)