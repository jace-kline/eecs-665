{-# LANGUAGE GADTs #-}
module ParseLexerSpec where

import StringParser

type StringParser a = Parser Char a

type RegexUnaryOp = Char
type NotNewlineChar = Char
type EscChar = Char
type NormalChar = Char
type AnyChar = Char
type QuotedMessage = String
type TokenName = String

data SpecParse where 
    SpecParse_ :: RuleParse -> SpecParse -> SpecParse
    SpecParseBase_ :: RuleParse -> SpecParse

newtype SpecParse = SpecParse { toRules :: [RuleParse] }

specParser :: StringParser SpecParse
specParser = fmap SpecParse $ oneOrMore ((parseRuleParser <* parseNewline) <|> parseRuleParser)

data RuleParse = RuleParse RegexParse ActionParse

parseRuleParser :: StringParser RuleParse
parseRuleParser = pure RuleParse <*> regexParser <**> actionParser

newtype RegexParse = RegexParse { toTerms :: [RegexTerm] }

regexParser :: StringParser RegexParse
regexParser = fmape RegexParse $ oneOrMore regexTermParser

newtype RegexTerm = RegexTerm { toBunches :: [RegexBunch] }

regexTermParser :: StringParser RegexTerm
regexTermParser = fmap RegexTerm $ oneOrMore regexBunchParser

data RegexBunch = BunchGroup RegexGroup RegexUnaryOp
                | BunchBase RegexGroup

regexBunchParser :: StringParser RegexBunch
regexBunchParser = cons1 <|> cons2 <|> empty
    where
        cons1 = pure BunchGroup <*> regexGroupParser <*> regexUnaryOpParser
        cons2 = pure BunchBase <*> regexGroupParser

data RegexGroup = GroupChar RegexChar
                | Grouping RegexParse
                | GroupQuoted [AnyChar]

regexGroupParser :: StringParser RegexGroup
regexGroupParser = cons1 <|> cons2 <|> cons3 <|> fail "Failed to parse RegexGroup"
    where
        cons1 = pure GroupChar <*> regexCharParser
        cons2 = pure Grouping <*> regexParser
        cons3 = pure GroupQuoted <*> zeroOrMore anyCharParser

data CharPrimitive = Chr Char
                    | EscSeq EscChar
                    deriving (Show, Eq)

data EscChar = Tab | Backslash | Singlequote | Doublequote | Newline | Space
    deriving (Show, Eq)

toEscChar :: String -> Either String (EscCharToken, String)
toEscChar [] = Left "No characters to parse"
toEscChar (c:cs) 
            | (c == '\t') = f Tab
            | (c == '\"') = f Doublequote
            | (c == '\'') = f Singlequote
            | (c == '\n') = f Newline
            | (c == '\\') = case cs of
                []       -> f Backslash
                (' ':_)  -> f Backslash
                ('_':ys) -> return (Space, ys)
            | otherwise = Left "No escape character match"
                where f t = return (t, cs)

escCharParser :: StringParser EscCharToken
escCharParser = Parser $ \s -> toEscChar s

charPrimitiveParser :: StringParser CharPrimitive
charPrimitiveParser = pars

anyCharParser :: StringParser Char
anyCharParser = normalCharParser <|> escCharParser <|> fail "Could not parse character"

normalCharParser :: StringParser Char
normalCharParser = p <|> fail "Could not parse a single character"
    where
        p = Parser $ \(c:cs) -> if isNormal c then return (c, cs) else Left "Invalid character parsed"
        isNormal c = isAscii c && isPrint c

escSeqParser :: StringParser EscapeSeq
escSeqParser = Parser $ \ts ->
    case matchFront ts escCharSeqs of
        Nothing -> Left "No escape sequence character match"
        Just s  -> Right $ (map toEscConstructor s, drop (length s) ts)

data RegexChar where
    RegexCharCharClass_ :: CharClass -> RegexChar
    RegexCharEscChar_ :: EscChar -> RegexChar
    RegexCharNormalChar_ :: NormalChar -> RegexChar
    RegexCharMatchAny_ :: NotNewlineChar -> RegexChar

data CharClass where
    CharClassNot_ :: CharClassBody -> CharClass
    CharClass_ :: CharClassBody -> CharClass

data CharClassBody where
    CharClassBody_ :: CharClassBody -> CharClassTerm -> CharClassBody
    CharClassBodyBase_ :: CharClassTerm -> CharClassBody

data CharClassTerm where
    CharClassTermNormalChar_ :: NormalChar -> CharClassTerm
    CharClassTermEscChar_ :: EscChar -> CharClassTerm
    CharClassTermQuoted_ :: AnyChar -> CharClassTerm
    CharClassTermRange_ :: CharRange -> CharClassTerm

data CharRange where
    CharRange_ :: NormalChar -> NormalChar -> CharRange_

data ActionParse where
    Directive_ :: ActionDirective -> ActionParse
    TokenDef_ :: ActionTokenDef -> ActionParse

data ActionDirective where
    Skip :: ActionDirective
    Err :: QuotedMessage -> ActionDirective

data ActionTokenDef where
    ActionTokenDef :: TokenName -> Bool -> ActionTokenDef