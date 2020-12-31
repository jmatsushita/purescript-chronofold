module Data.Chronofold.Types where

import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Eq (genericEq)
import Data.Generic.Rep.Show (genericShow)
import Data.Map (Map)
import Data.Maybe (Maybe)
import Data.String (CodePoint)

import Prelude (class Eq, class Ord, class Show, compare, eq, show, ($), (&&), (+), (<>), (==))

-- |
-- | processes α β γ in the paper, for me replica is better than process/site/author
-- |
newtype Replica = Replica Int
derive newtype instance showReplica :: Show Replica
derive newtype instance ordReplica :: Ord Replica
derive newtype instance eqReplica :: Eq Replica

type ReplicaIndex = Int
-- derive newtype instance showReplicaIndex :: Show ReplicaIndex

-- type Timestamp = Replica /\ ReplicaIndex 
data Timestamp = Timestamp Replica ReplicaIndex
instance showTimestamp :: Show Timestamp where
  show (Timestamp (Replica 1) i) = "α" <> show i
  show (Timestamp (Replica 2) i) = "β" <> show i
  show (Timestamp (Replica 3) i) = "γ" <> show i
  show (Timestamp (Replica r) i) = "rep" <> show r <> "/" <> show i
instance eqTimestamp :: Eq Timestamp where
  eq (Timestamp a i) (Timestamp b j) = eq a b && eq i j
instance ordTimestamp :: Ord Timestamp where
  compare (Timestamp a i) (Timestamp b j) = (compare a b) <> (compare i j)

-- from the paper
-- auth :: Timestamp -> Replica
-- auth = snd

-- andx :: Timestamp -> ReplicaIndex
-- andx = fst

-- newtype Value = Value CodePoint
-- derive instance newTypeValue :: Newtype Value _
-- derive newtype instance showValue :: Show Value

------------------------------------------
------------------------------------------
------------------------------------------


-- τ    0   1  2   3 
-- α  :    α6
--        (6α)
--           \
-- β  :       α6  
-- tβ        (6β)     

-- log of indices is the local coordinate system
-- log of timestamp is the shared coordinate system

-- 
-- chronofold :: ndx -> <val,nxt>
-- ndx       1α 2α 3α 4α 5α 6α
-- val        0  P  I  N  S  K

-- Maybe we want a Natural index or an Array parameterised by it's index type
-- type Chronofold = Array ({ codepoint :: Value, next :: Int })
-- in practice we use a thinned chronofold and offload the linked list management to a separate co-structure
type Chronofold = Array CodePoint

data Index = Index Int | Infinity
instance showIndex :: Show Index where
  show Infinity = "∞"
  -- show (Index i) = "Index " <> show i
  show (Index i) = show $ i + 1
instance eqIndex :: Eq Index where
  eq (Index i) (Index j) = i == j
  eq (Infinity) (Infinity) = true
  eq _ _ = false

-- nxt       2α 3α 4α 5α 6α  ⊤ 

-- This could be a sparse array (Offset map in the Rust implementation https://github.com/dkellner/chronofold/blob/main/src/lib.rs#L174)
type Next = Array Index

-- co-structures
-- ndx :: t -> ndx
-- t         α1 α2 α3 α4 α5 α6 β1 
-- ndx       1α 2α 3α 4α 5α 6α 7α 

-- Ndx is not materialised in the paper, instead it scans NdxInv or
-- uses a shift table (a separate table of index shifts).
-- Map Timestamp Index
type Ndx = Map Timestamp Int -- (α1, 1)
-- type Ndx = Map Author (Array Index) -- (α, [1])

-- ndx⁻¹ :: ndx -> t 
-- ndx       1α 2α 3α 4α 5α 6α 7
-- t         α1 α2 α3 α4 α5 α6 β1

type NdxInv = Array Timestamp

-- ref :: timestamp -> timestamp
-- t         α1 α2 α3 α4 α5 α6
-- ref(t)     0 α1 α2 α3 α4 α5

-- ref is conceptually a function from timestamp to timestamp
-- we turn it around to have parents as keys to detect CT siblings.
-- Ref⁻¹ 
type Ref = Map (Maybe Timestamp) (Array Timestamp)

-- op
-- t            α2
-- ref          α1    
-- val           0

-- An op is what transmitted across replicas and is uniquely identified by it's timestamp t = <i, β> .
-- An op is a tuple ⟨t,ref(t), val(t)⟩ 
-- op
-- t    α2    -- uniquely identified by it's timestamp t = <i, β>
-- ref  α1    
-- val   P

--               t           ref            val
data Op = Op Timestamp (Maybe (Timestamp)) CodePoint
derive instance genericOp :: Generic Op _
instance showOp :: Show Op where show = genericShow
             
-- |             thinned Chonofold
-- |                     |                   
-- |  current timestamp  |     weave       causal tree
-- |          |          |       |              |
-- |          v          v       v              v
-- | Log Timestamp Chronofold Next Ndx NdxInv Ref 
data Log = Log Timestamp Chronofold Next Ndx NdxInv Ref 
derive instance genericLog :: Generic Log _
instance showLog ::  Show Log where show = genericShow
instance eqLog ::  Eq Log where eq = genericEq

-- instance showLog :: Show Log where
--   show (Log l) = show l

-- class Monoid w <= Action w s where
--   act :: w -> s -> s

-- instance actionLog :: Action Op Log where
--   act o l = appendOp l o

-- class (Action w s, Monad m) <= MonadUpdate m w s | m -> s , m -> w
-- instance monadUpdateLog :: MonadUpdate (UpdateState Op Log) Op Log where
--     -- putAction :: w -> m Unit
--     putAction :: Op -> UpdateState Unit 
--     putAction op = pure unit
--     -- getState :: m s
--     getState :: UpdateState Log
--     getState = pure $ Log { replica : Replica 1, ops : []}
