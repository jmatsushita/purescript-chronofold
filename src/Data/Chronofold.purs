module Data.Chronofold 
  ( module Data.Chronofold.Core
  , module Data.Chronofold.Types
  , module Data.Chronofold.Buffer) where

import Data.Chronofold.Core (appendOp, appendOps, buildCausalOp, buildCausalStringOps, buildSnocOp, buildSnocStringOps, emptyLog, ndxInv, ndxInvEndsWith, root)
import Data.Chronofold.Buffer (naiveProject, project)
import Data.Chronofold.Types (Chronofold, Index(..), Log(..), Ndx, NdxInv, Next, Op(..), Ref, Replica(..), ReplicaIndex, Timestamp(..))

-- | Conceptually we have:
-- | - a "shared" reference/space, in which 
-- |   we're aware of all replicas. `Op`s exist in this space and
-- |   use replica indexed references (Log timestamps).
-- | - a "local" space, in each replica (Log indices)
-- | - a "local screen/buffer" space, where Chronofolds are "rendered"
-- |   and which eventually converge by construction.
