module Data.Chronofold 
  ( module Data.Chronofold.Core
  , module Data.Chronofold.Types
  , module Data.Chronofold.Buffer) where

import Data.Chronofold.Core
import Data.Chronofold.Buffer
import Data.Chronofold.Types

-- | Conceptually we have:
-- | - a "shared" reference/space, in which 
-- |   we're aware of all replicas. `Op`s exist in this space and
-- |   use replica indexed references (Log timestamps).
-- | - a "local" space, in each replica (Log indices)
-- | - a "local screen/buffer" space, where Chronofolds are "rendered"
-- |   and which eventually converge by construction.
