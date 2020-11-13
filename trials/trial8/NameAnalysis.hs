{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
module NameAnalysis where
-- parse AST chunks to check names
-- must reference program state

import AST
import ProgState