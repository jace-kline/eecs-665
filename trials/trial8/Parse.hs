{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
module Parse where
-- parse token sequence to AST structures

import Token
import AST
import Parser
import Control.Monad
import Control.Monad.Fail
import Control.Applicative

type TokenParser a = Parser Token a

-- Parser --
parser :: [Token] -> Maybe [Effect]
parser ts = evalParser effectsParser ts

-- Utility --

prim :: Prim -> TokenParser Prim
prim p = token (PrimToken p) >>= \(PrimToken p') -> return p'

commaSep :: TokenParser a -> TokenParser [a]
commaSep p = ((:) <$> (p <* prim COMMA_) <*> commaSep p) <|> ((\x -> [x]) <$> p) <|> return []

fromMaybe (Just x) = x

-- DType parsers --

primToDT :: Prim -> DType
primToDT INT_ = IntT
primToDT BOOL_ = BoolT
primToDT CHAR_ = CharT
primToDT CHARPTR_ = CharPtrT
primToDT VOID_ = VoidT
primToDT _ = error "Bad value"

dtParser :: [Prim] -> TokenParser DType
dtParser prims = do
    p <- sequenceAlt (map prim prims)
    return $ primToDT p

dtypePrims = [VOID_, INT_, BOOL_, CHAR_, CHARPTR_]

dtypeParser :: TokenParser DType
dtypeParser = dtParser (tail dtypePrims)

dtypeParserWithVoid :: TokenParser DType
dtypeParserWithVoid = dtParser dtypePrims
-- Effect (declaration/statement) parsers --

effParsers :: [TokenParser Effect]
effParsers = [
    fnDeclParser,
    varDeclParser,
    assignStmtParser,
    decStmtParser,
    incStmtParser,
    fromConsoleParser,
    toConsoleParser,
    ifElseParser,
    whileParser,
    returnParser,
    fnCallStmtParser]

effectsParser :: TokenParser [Effect]
effectsParser = do
    effs <- many effectParser
    rest <- look
    guard (null rest)
    return effs

effectParser :: TokenParser Effect
effectParser = sequenceAlt effParsers

effectBodyParser :: TokenParser Effect
effectBodyParser = sequenceAlt $ tail effParsers

varDeclParser :: TokenParser Effect
varDeclParser = do
    t <- dtypeParser
    (ID id) <- idParser
    prim SEMICOLON_
    return $ VarDecl id t

fnDeclParser :: TokenParser Effect
fnDeclParser = do
    rt <- dtypeParserWithVoid
    (ID id) <- idParser
    prim LPAREN_
    formals <- commaSep formalParser
    prim RPAREN_
    prim LCURLY_
    body <- many effectBodyParser
    prim RCURLY_
    return $ FnDecl id rt formals body

formalParser :: TokenParser (Id, DType)
formalParser = do
    t <- dtypeParser
    (ID id) <- idParser
    return (id, t)

assignStmtParser :: TokenParser Effect
assignStmtParser = do
    e <- assignExpParser
    prim SEMICOLON_
    case e of
        (_ := _) -> return $ AssignStmt e
        _ -> empty

decStmtParser :: TokenParser Effect
decStmtParser = do
    (ID id) <- idParser
    prim DEC_
    prim SEMICOLON_
    return $ Dec id

incStmtParser :: TokenParser Effect
incStmtParser = do
    (ID id) <- idParser
    prim INC_
    prim SEMICOLON_
    return $ Inc id

fromConsoleParser :: TokenParser Effect
fromConsoleParser = do
    prim FROMCONSOLE_
    (ID id) <- idParser
    prim SEMICOLON_
    return $ FromConsole id

toConsoleParser :: TokenParser Effect
toConsoleParser = do
    prim TOCONSOLE_
    e <- expParser
    prim SEMICOLON_
    return $ ToConsole e

branch :: Prim -> TokenParser (Exp, [Effect])
branch p = do
    prim p
    prim LPAREN_
    c <- expParser
    prim RPAREN_
    prim LCURLY_
    effs <- many effectBodyParser
    prim RCURLY_
    return $ (c, effs)

ifElseParser :: TokenParser Effect
ifElseParser = do
    (c, effs) <- branch IF_
    elseParser c effs <|> return (If c effs)
    where
        elseParser c effs = do
            prim ELSE_
            prim LCURLY_
            effs' <- many effectBodyParser
            prim RCURLY_
            return $ IfElse c effs effs'

whileParser :: TokenParser Effect
whileParser = branch WHILE_ >>= \(c, effs) -> return $ While c effs

returnParser :: TokenParser Effect
returnParser = do
    prim RETURN_
    maybe_e <- optional expParser
    prim SEMICOLON_
    return $ Return maybe_e

fnCallStmtParser :: TokenParser Effect
fnCallStmtParser = (FnCallStmt <$> fnCallParser) <* prim SEMICOLON_

-- Exp parsers --

expParser :: TokenParser Exp
expParser = assignExpParser

assignExpParser :: TokenParser Exp
assignExpParser = p <|> orExpParser
    where p = do
            (ID id) <- idParser
            prim ASSIGN_
            e <- expParser
            return (id := e)

orExpParser :: TokenParser Exp
orExpParser = p <|> andExpParser
    where p = do
            l <- andExpParser
            prim OR_
            r <- orExpParser
            return (l :||: r)

andExpParser :: TokenParser Exp
andExpParser = p <|> cmpExpParser
    where p = do
            l <- cmpExpParser
            prim AND_
            r <- andExpParser
            return (l :&&: r)

binOpParser :: [(Exp -> Exp -> Exp)] -> [Prim] -> TokenParser Exp -> TokenParser Exp -> TokenParser Exp
binOpParser ops prims pleft pright = (sequenceAlt parsers) <|> pleft
    where
        primToOp :: Prim -> (Exp -> Exp -> Exp)
        primToOp p = fromMaybe $ lookup p $ zip prims ops
        parsers :: [TokenParser Exp]
        parsers = map exp prims
        exp :: Prim -> TokenParser Exp
        exp p = do
            l <- pleft
            prim p
            r <- pright
            return $ (primToOp p) l r

cmpExpParser :: TokenParser Exp
cmpExpParser = binOpParser ops prims p p
    where
        ops = [(:>:), (:>=:), (:<:), (:<=:), (:==:), (:!=:)]
        prims = [GT_, GEQ_, LT_, LEQ_, EQ_, NEQ_]
        p = arithExpParser

arithExpParser :: TokenParser Exp
arithExpParser = binOpParser ops prims pleft pright
    where
        ops = [(:+:), (:-:)] 
        prims = [CROSS_, DASH_]
        pleft = prodExpParser 
        pright = arithExpParser

prodExpParser :: TokenParser Exp
prodExpParser = binOpParser ops prims pleft pright
    where
        ops = [(:*:), (:/:)]
        prims = [STAR_, SLASH_]
        pleft = termExpParser
        pright = prodExpParser

termExpParser :: TokenParser Exp
termExpParser = negParser <|> notParser <|> unitExpParser

negParser :: TokenParser Exp
negParser = do
    prim DASH_
    e <- expParser
    return $ Neg e

notParser :: TokenParser Exp 
notParser = do
    prim BANG_
    e <- expParser
    return $ Not e

unitExpParser :: TokenParser Exp
unitExpParser = idParser <|> litParser <|> fnCallParser <|> grp
    where
        grp = prim LPAREN_ *> expParser <* prim RPAREN_

litParser :: TokenParser Exp
litParser = boolLit <|> charLit <|> intLit <|> strLit
    where
        boolLit = do
            (PrimToken p) <- get
            if (p == TRUE_ || p == FALSE_)
                then return $ BoolLit (toBool p)
                else empty
            where
                toBool TRUE_ = True
                toBool FALSE_ = False
        charLit = do
            (CharLitToken c) <- get
            return $ CharLit c
        intLit = do
            (IntLitToken n) <- get
            return $ IntLit n
        strLit = do
            (StrLitToken s) <- get
            return $ StrLit s

fnCallParser :: TokenParser Exp
fnCallParser = do
    (ID id) <- idParser
    prim LPAREN_
    actuals <- commaSep expParser
    prim RPAREN_
    return $ FnCall id actuals

idParser :: TokenParser Exp
idParser = do
    (IdToken i) <- get
    return $ ID i