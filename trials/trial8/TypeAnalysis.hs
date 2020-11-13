{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
module TypeAnalysis where
-- fold over a chunk of AST code and type check
-- follows name analysis
-- must reference program state

import AST
import ProgState