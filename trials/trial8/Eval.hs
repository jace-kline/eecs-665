{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
module Eval where
-- fold over the desired abstract syntax to produce
-- a change in program state

import AST
import ProgState