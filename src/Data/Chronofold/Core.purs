module Data.Chronofold.Core where
  
-- import Control.Monad.Update
-- import Data.Monoid.Action

import Data.Array (insertAt, length, snoc, unsnoc, updateAt, (!!))
import Data.Array as Array
import Data.Bounded (bottom)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Eq (genericEq)
import Data.Generic.Rep.Show (genericShow)
import Data.Map (Map, empty, insert, lookup, member)
import Data.Maybe (Maybe(..))
import Data.Newtype (overF)
import Data.String (CodePoint)
import Effect.Exception (error)
import Effect.Exception.Unsafe (unsafeThrowException)
import Prelude (class Eq, class Ord, class Show, compare, eq, show, ($), (&&), (+), (-), (<>), (==), bind, pure)

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


-- data Log = Log ReplicaIndex Chronofold Next NextInv Ref 
-- Op  t ref val

ndxInvEndsWith :: Timestamp -> Array Timestamp -> Boolean
ndxInvEndsWith (Timestamp r i) ts = case unsnoc ts of
  Just {init:_, last: Timestamp r' j} -> j == i -- unsafeThrowException (error $ show j <> "/" <> show i) 
  Nothing -> true -- Maybe we want to check that we're inserting the root Op here

-- | Appends an `Op` 
-- | ```purescript
-- | >>> logShow $ appendOp (emptyLog (Replica 1)) (Op (Timestamp alpha 2) (Just (Timestamp alpha 1)) (codePointFromChar 'P'))
-- | (Log α1 [(CodePoint 0x50)] [1,∞] (fromFoldable [(Tuple α1 0),(Tuple α2 1)]) [α1,α2] (fromFoldable [(Tuple α1 Nothing),(Tuple α2 (Just α1))]))
-- | ```
appendOp :: Log -> Op -> Log
appendOp 
  (Log (Timestamp r i) c next ndxM ndxinv ref) 
  op@(Op t@(Timestamp r' i') oref v) = 
    let 
      cur = length c -- current local index
      newNext = case oref of
        Nothing -> [Infinity]
        Just oref' -> case ndxInvEndsWith oref' ndxinv of
            true -> case insertAt (cur - 1) (Index $ cur) next of
              Just a   -> a
              Nothing -> unsafeThrowException (error "index out of bounds in next")
            -- we need to relink the linked list
            -- we mutate next[ndx(oref')]
            false -> case next'' of
                Just a   -> a
                Nothing -> unsafeThrowException (error $ "error relinking the linked list")
              where
                next'' = do
                  ndxIndex <- lookup oref' ndxM
                  ndxSwap <- case ndxIndex of
                    Index n -> Just n
                    Infinity -> Nothing
                  nxtSwap <- next !! (ndxSwap - 1)
                  -- could be a foreign import that swaps faster
                  next' <- updateAt (ndxSwap - 1) (Index $ cur) next
                  insertAt cur nxtSwap next'
              
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

-- ndx       1α 2α 3α 4α 5α 6α 7α
-- val        0  P  I  N  S  K 
-- nxt       2α 3α 4α 5α 6α  ∞  

-- t         α1 α2 α3 α4 α5 α6   -- morally ndx + ndx⁻¹   
-- ref        0 α1 α2 α3 α4 α5  

-- op
-- t                           β7 - <i,β>
-- ref                         α2 - ref(<i,β>) = <k,γ> / ref(<7,β>) = <2,α> αʲ where j = 2
-- val                         <=

-- ndx       1α 2α 3α 4α 5α 6α 7α 
-- val        0  P  I  N  S  K <= 

--            relinking the linked list                
--               v              v 
-- nxt       2α 7α 4α 5α 6α  ∞ 3α  

-- t         α1 α2 α3 α4 α5 α6 β7   
-- ref        0 α1 α2 α3 α4 α5 α2

-- op
-- t                               β8
-- ref                             β7
-- val                             M


-- ndx       1α 2α 3α 4α 5α 6α 7α 8α 
-- val        0  P  I  N  S  K <=  M

--            relinking the linked list                
--                              v  v 
-- nxt       2α 7α 4α 5α 6α  ∞ 8α 3α  

-- t         α1 α2 α3 α4 α5 α6 β7 β8
-- ref        0 α1 α2 α3 α4 α5 α2 β7

-- |
-- | Appends an `Op` without checking invariants.
-- |
-- unsafeAppendOp :: Log -> Op -> Log
-- unsafeAppendOp 

appendOps :: Log -> Array Op -> Log
appendOps = Array.foldl appendOp

-- |
-- | Build an `Op` which appends the `CodePoint` at the end of the weave.
-- |
buildSnocOp :: Log -> CodePoint -> Op
buildSnocOp log@(Log (Timestamp rep i) a b c d e) val =
  Op (Timestamp rep (i + 1)) (Just (Timestamp rep (i))) val

-- |
-- | Build an `Op` which inserts the `CodePoint` after the `Timestamp`,
-- | i.e. the `ref` in the causal tree structure.
-- |
buildCausalOp :: Log -> Timestamp -> CodePoint -> Op
buildCausalOp log@(Log (Timestamp rep i) a b c d e) t val =
  Op (Timestamp rep (i + 1)) (Just t) val

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

-- alphaLogRoot = appendOp (emptyLog alpha) $ root alpha
-- ndx       1α 
-- val        0  
-- nxt        ∞     
-- t         α1 
-- ref        0