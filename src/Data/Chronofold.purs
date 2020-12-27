module Data.Chronofold where
  
-- import Control.Monad.Update
-- import Data.Monoid.Action


import Data.Array (foldl, insertAt, length, snoc)
import Data.Array as Array
import Data.Bounded (bottom)
import Data.Enum (fromEnum)

import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Eq (genericEq)
import Data.Generic.Rep.Show (genericShow)
import Data.Map (Map, empty, insert, member)
import Data.Maybe (Maybe(..))
import Data.String (CodePoint, singleton, splitAt, toCodePointArray)
import Data.String (length) as S

import Effect.Exception (error)
import Effect.Exception.Unsafe (unsafeThrowException)
import Prelude (class Eq, class Ord, class Show, compare, eq, show, ($), (&&), (+), (-), (<>), (==))


-- processes α β γ in the paper, for me replica is better than process/site/author
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
  show (Index i) = "Index " <> show i
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

-- Map Timestamp Index
type Ndx = Map Timestamp Index -- (α1, 1)
-- type Ndx = Map Author (Array Index) -- (α, [1])

-- ndx⁻¹ :: ndx -> t 
-- ndx       1α 2α 3α 4α 5α 6α 7
-- t         α1 α2 α3 α4 α5 α6 β1

type NdxInv = Array Timestamp

-- ref :: timestamp -> timestamp
-- t         α1 α2 α3 α4 α5 α6
-- ref(t)     0 α1 α2 α3 α4 α5

-- ref is conceptually a function from timestamp to timestamp
-- Map Timestamp Timestamp
type Ref = Map Timestamp (Maybe Timestamp)

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
                   
--                    thinned Chonofold
--                            |                   
--         current timestamp  |     weave       causal tree
--                 |          |       |              |
--                 v          v       v              v
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


-- data Log = Log ReplicaIndex Chronofold Next NextInv Ref 
-- Op  t ref val
-- | Appends an `Op` 
-- | ```purescript
-- | >>> logShow $ appendOp (emptyLog (Replica 1)) (Op (Timestamp alpha 2) (Just (Timestamp alpha 1)) (codePointFromChar 'P'))
-- | (Log α1 [(CodePoint 0x50)] [1,∞] (fromFoldable [(Tuple α1 0),(Tuple α2 1)]) [α1,α2] (fromFoldable [(Tuple α1 Nothing),(Tuple α2 (Just α1))]))
-- | ```

-- data Log = Log ReplicaIndex Chronofold Next NextInv Ref 
-- emptyLog :: Replica -> Log
-- emptyLog rep = Log 
--   (Timestamp rep 0) 
--   [] 
--   [] 
--   empty 
--   [] 
--   empty

-- root :: Replica -> Op
-- root rep = Op (Timestamp rep 1) Nothing bottom

appendOp :: Log -> Op -> Log
appendOp (Log (Timestamp r i) c next ndxM ndxinv ref) (Op t@(Timestamp r' i') oref v) = 
  let 
    cur = length c -- current local index
    newNext = case oref of
      Nothing -> [Infinity]
      _ -> case insertAt (cur - 1) (Index $ cur) next of
            Just a   -> a
            Nothing -> unsafeThrowException (error "index out of bounds in next")
    newNdxM = case member t ndxM of
                  true -> unsafeThrowException (error "didn't expect the timestamp to already exist in ndx")
                  false -> insert t (Index $ cur + 1) ndxM
    newRef = case member t ref of
                  true -> unsafeThrowException (error "didn't expect the timestamp to already exist in ref")
                  false -> insert t oref ref
    newNdxInv = case (insertAt cur t ndxinv) of
      Just a  -> a
      Nothing -> unsafeThrowException (error "index out of bounds in ndx⁻¹")
  in 
    Log 
      (Timestamp r (i + 1))
      (snoc c v) 
      newNext
      newNdxM 
      newNdxInv
      newRef

-- |
-- | Appends an `Op` without checking invariants.
-- |
-- unsafeAppendOp :: Log -> Op -> Log
-- unsafeAppendOp 

appendOps :: Log -> Array Op -> Log
appendOps = Array.foldl appendOp

-- buildOp needs a cursor position
buildOp :: Log -> CodePoint -> Op
buildOp log@(Log (Timestamp rep i) a b c d e) val =
  Op (Timestamp rep (i + 1)) (Just (Timestamp rep (i))) val

appendString :: Log -> String -> Log
appendString l s = 
  let chars = toCodePointArray s
  in  foldl (\l' c -> appendOp l' $ buildOp l' c) l chars 

-- type Chronofold = Array Value

-- data Log = Log ReplicaIndex Chronofold Next NextInv Ref 
-- emptyLog :: Replica -> Log
-- emptyLog rep = Log 
--   (Timestamp rep 1) 
--   [] 
--   [Infinity] 
--   (insert (Timestamp rep 1) (Index 0) empty) 
--   [(Timestamp rep 1)] 
--   (insert (Timestamp rep 1) Nothing empty)

emptyLog :: Replica -> Log
emptyLog rep = Log 
  (Timestamp rep 0) 
  [] 
  [] 
  empty 
  [] 
  empty

root :: Replica -> Op
root rep = Op (Timestamp rep 1) Nothing bottom

-- interpret the log into a String.
-- project :: Log -> GPU TypedArray
-- diffable project (incremental). Array Op -> Mutation TypedArray 

naiveProject :: Log -> String
naiveProject (Log _ cps _ _ _ _) = foldl go "" cps
  where
    go :: String -> CodePoint -> String
    go b a = case fromEnum a of 
      0x0000 -> b
      0x0008 -> (splitAt ((S.length b) - 1) b).before
      _      -> b <> singleton a

