{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
module Token where

data Token where
    PrimToken :: Prim -> Token
    CharLitToken :: Char -> Token
    IntLitToken :: Int -> Token
    StrLitToken :: String -> Token
    IdToken :: String -> Token
    deriving (Show, Eq)

data Prim where
    INT_ :: Prim
    BOOL_ :: Prim
    CHARPTR_ :: Prim
    CHAR_ :: Prim
    VOID_ :: Prim
    IF_ :: Prim
    ELSE_ :: Prim
    WHILE_ :: Prim
    RETURN_ :: Prim
    TRUE_ :: Prim
    FALSE_ :: Prim
    FROMCONSOLE_ :: Prim
    TOCONSOLE_ :: Prim
    LCURLY_ :: Prim
    RCURLY_ :: Prim
    LPAREN_ :: Prim
    RPAREN_ :: Prim
    SEMICOLON_ :: Prim
    COMMA_ :: Prim
    DEC_ :: Prim
    DASH_ :: Prim
    INC_ :: Prim
    CROSS_ :: Prim
    STAR_ :: Prim
    SLASH_ :: Prim
    EQ_ :: Prim
    NEQ_ :: Prim
    BANG_ :: Prim
    LEQ_ :: Prim
    LT_ :: Prim
    GEQ_ :: Prim
    GT_ :: Prim
    ASSIGN_ :: Prim
    AND_ :: Prim
    OR_ :: Prim
    deriving (Eq, Show, Enum)


allPrims :: [Prim]
allPrims = map (toEnum :: Int -> Prim) [fromEnum start .. fromEnum end]
    where
        start = INT_
        end = OR_

repr :: Prim -> String
repr INT_ = "int"
repr BOOL_ = "bool"
repr CHAR_ = "char"
repr CHARPTR_ = "charptr"
repr VOID_ = "void"
repr IF_ = "if"
repr ELSE_ = "else"
repr WHILE_ = "while"
repr RETURN_ = "return"
repr TRUE_ = "true"
repr FALSE_ = "false"
repr FROMCONSOLE_ = "FROMCONSOLE"
repr TOCONSOLE_ = "TOCONSOLE"
repr LCURLY_ = "{"
repr RCURLY_ = "}"
repr LPAREN_ = "("
repr RPAREN_ = ")"
repr SEMICOLON_ = ";"
repr COMMA_ = ",";
repr DEC_ = "--";
repr DASH_ = "-";
repr INC_ = "++";
repr CROSS_ = "+";
repr STAR_ = "*";
repr SLASH_ = "/";
repr EQ_ = "==";
repr NEQ_ = "!=";
repr BANG_ = "!";
repr LEQ_ = "<=";
repr LT_ = "<";
repr GEQ_ = ">=";
repr GT_ = ">";
repr ASSIGN_ = "=";
repr AND_ = "&&";
repr OR_ = "||";