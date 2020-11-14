{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
module ProgState where
-- keep a running state of the program while interpreting

import AST

-- Fn type + accessors
type Fn = (RetType, [(Id, ArgType)], [Effect])

mkFn :: Effect -> (Id, Fn)
mkFn (FnDecl id rt formals es) = (id, (rt, formals, es))
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
    parent :: Maybe Scope -- if parent, Nothing
} deriving (Eq, Show)


globalScope :: Scope
globalScope = Scope {
    funcs = [],
    gblTypes = [],
    gblVals = [],
    localTypes = [],
    localVals = [],
    retType = VoidT,
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
                  parent = return s}

-- return Just parent with modified global values
-- if no parent, return Nothing
popScope :: Scope -> Maybe Scope
popScope s = do
    p <- parent s
    return $ p {gblVals = gblVals s}

-- lookup functions

getFn :: Id -> Scope -> Maybe Fn
getFn id s = lookup id $ funcs s

getType :: Id -> Scope -> Maybe DType
getType id s = lookup id $ combTypes s
    where 
        combTypes :: Scope -> [(Id, DType)]
        combTypes s = localTypes s ++ gblTypes s

getVal :: Id -> Scope -> Maybe Exp
getVal id s = lookup id $ combVals s
    where
        combVals :: Scope -> [(Id, Exp)]
        combVals s = localVals s ++ gblVals s

-- modification functions

putFn :: (Id, Fn) -> Scope -> Scope
putFn fn s@(Scope {funcs = fns}) = s {funcs = (fn:fns)}

putType :: (Id, DType) -> Scope -> Scope
putType v s@(Scope {gblTypes = gts, localTypes = lts}) = if isGblScope s 
                                                                then s {gblTypes = (v : gts)}
                                                                else s {localTypes = (v : lts)}

putVal :: (Id, Exp) -> Scope -> Scope
putVal v@(id,e) s@(Scope {gblVals = gvs, localVals = lvs}) =
    let putGbl = s {gblVals = (v:gvs)}
    in if isGblScope s
        then putGbl
        else case lookup id lvs of
            Nothing -> putGbl
            Just _ -> s {localVals = (v:lvs)}