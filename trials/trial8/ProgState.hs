{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
module ProgState where
-- keep a running state of the program while interpreting

import AST

-- Fn type + accessors
type Fn = (RetType, [(Id, ArgType)], [Effect])

mkFn :: Effect -> Fn
mkFn (FnDecl id rt formals es) = (rt, formals, es)
mkFn _ = error "Tried to convert non-function to function"

fnRetType :: Fn -> RetType
fnRetType (t,_,_) = t

fnFormals :: Fn -> [(Id, ArgType)]
fnFormals (_,xs,_) = xs

fnEffects :: Fn -> [Effect]
fnEffects (_,_,es) = es

-- Scope type : the state node of a program scope
data Scope = Scope {
    funcs :: [(Id, Fn)],
    gblTypes :: [(Id, DType)],
    gblVals :: [(Id, Exp)],
    localTypes :: [(Id, DType)],
    localVals :: [(Id, Exp)],
    retType :: RetType,
    retVal :: Maybe Exp, -- set this when hitting a return stmt
    haveReturned :: Bool, -- if scope has hit a return
    parent :: Maybe Scope -- if parent, Nothing
} deriving (Eq, Show)


initScope :: Scope
initScope = Scope {
    funcs = [],
    gblTypes = [],
    gblVals = [],
    localTypes = [],
    localVals = [],
    retType = VoidT,
    retVal = Nothing,
    haveReturned = False,
    parent = Nothing
}

isGblScope :: Scope -> Bool
isGblScope s = case parent s of
    Just _ -> False
    Nothing -> True

-- create a new scope from a function description
newScope :: Fn -> Scope -> Scope
newScope f s = s {localTypes = fnFormals f, 
                  localVals = [], 
                  retType = fnRetType f,
                  retVal = Nothing,
                  haveReturned = False,
                  parent = return s}

-- return Just parent with modified global values
-- if no parent, return Nothing
popScope :: Scope -> Maybe Scope
popScope s = do
    p <- parent s
    return $ p {gblVals = gblVals s,
                retVal = retVal s}

-- lookup functions

look :: (Scope -> [(Id, a)]) -> Id -> Scope -> Maybe a
look f id s = lookup id $ f s

getFn :: Id -> Scope -> Maybe Fn
getFn = look funcs

getLocalType :: Id -> Scope -> Maybe DType
getLocalType = look localTypes

getGblType :: Id -> Scope -> Maybe DType
getGblType = look gblTypes

getType :: Id -> Scope -> Maybe DType
getType = look combTypes

combTypes :: Scope -> [(Id, DType)]
combTypes s = localTypes s ++ gblTypes s

getLocalVal :: Id -> Scope -> Maybe Exp
getLocalVal = look localVals

getGblVal :: Id -> Scope -> Maybe Exp
getGblVal = look gblVals

getVal :: Id -> Scope -> Maybe Exp
getVal = look combVals

combVals :: Scope -> [(Id, Exp)]
combVals s = localVals s ++ gblVals s

-- modification functions

putFn :: Id -> Fn -> Scope -> Scope
putFn id fn s@(Scope {funcs = fns}) = s {funcs = ((id, fn):fns)}

putType :: Id -> DType -> Scope -> Scope
putType id t s@(Scope {gblTypes = gts, localTypes = lts}) = if isGblScope s 
                                                                then s {gblTypes = ((id,t) : gts)}
                                                                else s {localTypes = ((id,t) : lts)}

putVal :: Id -> Exp -> Scope -> Scope
putVal id e s@(Scope {gblVals = gvs, localVals = lvs}) =
    let putGbl = s {gblVals = ((id,e):gvs)}
    in if isGblScope s
        then putGbl
        else case lookup id lvs of
            Nothing -> putGbl
            Just _ -> s {localVals = ((id,e):lvs)}

putRet :: Maybe Exp -> Scope -> Scope
putRet me s = s {retVal = me}

setHaveReturned :: Scope -> Scope
setHaveReturned s = s {haveReturned = True}