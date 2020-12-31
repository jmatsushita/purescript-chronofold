module Data.Chronofold.Core where
  
-- import Control.Monad.Update
-- import Data.Monoid.Action

import Data.Chronofold.Types

import Control.Applicative ((<$>), (<*>))
import Data.Array (drop, findLastIndex, insertAt, length, snoc, uncons, unsnoc, updateAt, (!!))
import Data.Array as Array
import Data.Bounded (bottom)

import Data.Enum (fromEnum)
import Data.Map (empty, insert, lookup, member)
import Data.Maybe (Maybe(..), maybe)

import Data.String (CodePoint, toCodePointArray)

import Effect.Exception (error)
import Effect.Exception.Unsafe (unsafeThrowException)
import Prelude (bind, identity, pure, show, ($), (+), (-), (<>), (==))



ndxInv :: Log -> Int -> Maybe Timestamp
ndxInv (Log _ _ _ _ ndxinv _) i = ndxinv !! i  
-- data Log = Log Timestamp Chronofold Next Ndx NdxInv Ref 
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
  log@(Log (Timestamp r i) c next ndxM ndxinv ref) 
  op@(Op t@(Timestamp r' i') opref v) = 
    let 
      end = length c -- index of the end of the local log
      newNext = case opref of
        Nothing -> [Infinity]
        Just opref' -> 
          -- We first test if we're at the end, if not we need to check for preemptive siblings
          case ndxInvEndsWith opref' ndxinv of
            -- we append at the end? 
            true -> case insertAt (end - 1) (Index $ end) next of
              Just a   -> a
              Nothing -> unsafeThrowException (error $ "index out of bounds in next" <> show log <> "/" <> show op)
            
            -- there might be a preemptive CT sibling
            false -> 
              if (fromEnum v == 0x0008) 
              -- deletes have priority, we don't need to follow the "subtree" 
              then case next'' of
                      Just a   -> a
                      Nothing -> unsafeThrowException (error $ "error relinking the linked list")

              else 
                case lookup opref newRef of
                  -- Are there CT siblings?
                  Just siblings -> -- [α3,β7]
                    -- We'll remove this map probably, but until then, we store 
                    -- the highest priority CT branch
                    case Array.elemIndex t siblings of
                      Just ourSiblingIndex ->
                        case siblings !! (ourSiblingIndex - 1) of 
                          -- that's our adjacent older sibling
                          -- the sibling has higher priority than us, so we insert after the 
                          -- CT branch of the preemptive sibling
                          -- Just priorityref -> case next'' of
                          --   Just a   -> a -- const (spy "next'''" a) (spy "op" op)
                          --   Nothing -> unsafeThrowException (error $ "error relinking the linked list")

                          Just priorityref -> case next''' priorityref of
                            Just a   -> a
                            Nothing -> unsafeThrowException (error $ "error relinking the linked list")
                          -- we have higher priority so we just insert and relink as usual.
                          Nothing -> 
                            case next'' of
                              Just a   -> a
                              Nothing -> unsafeThrowException (error $ "error relinking the linked list") 
                      Nothing -> unsafeThrowException (error $ "invariant violation. we just inserted t in newRef") 
                  -- There isn't a CT sibling
                  -- we need to relink the linked list
                  -- we mutate next[ndx(opref')]
                  Nothing -> case next'' of
                      Just a   -> a
                      Nothing -> unsafeThrowException (error $ "error relinking the linked list")

                  where 
                    next'' = do
                        ndxIndex <- lookup opref' ndxM
                        nxtSwap <- next !! ndxIndex
                        -- could be a foreign import that swaps faster
                        next' <- updateAt ndxIndex (Index $ end) next
                        insertAt end nxtSwap next'
                    next''' pref@(Timestamp r1 _) = do
                        ndxPriority <- lookup pref ndxM -- 2
                        ndxPriorityLast <- (+) <$> Just ndxPriority <*> (findLastIndex (\(Timestamp r1' _) -> r1 == r1') $ drop ndxPriority ndxinv)
                        nxtSwap <- next !! ndxPriorityLast
                        -- could be a foreign import that swaps faster
                        next' <- updateAt ndxPriorityLast (Index $ end) $ next
                        insertAt end nxtSwap $ next'


                    -- t         α1 α2 α3 α4 α5 α6 α7 α8 α9 β5 β6 β7
                    -- ref        0 α1 α2 α3 α4 α5 α6 α7 α8 α4 β5 β6

                    -- 0 - A(α1) - B(α2) - C(α3) - ∞ 
                    --       \ 
                    --         D(β2) - E(β3) - G(δ4)
                    --           \
                    --            F(γ3)

                    -- 0(α1) - C(α2) - M(α3) - D(α4) - <=(α5) - <=(α6) - T(α7) - R(α8) - L(α9) - ∞ 
                    --                             \ 
                    --                              D(β5) - E(β6) - L(β7)


              
      newNdxM = case member t ndxM of
                    true -> unsafeThrowException (error "didn't expect the timestamp to already exist in ndx")
                    false -> insert t end ndxM
      newRef = case lookup opref ref of
                Just siblings -> 
                  -- We'll remove this map probably, but until then, we store 
                  -- the highest priority CT branch
                  insert opref (Array.insert t siblings) ref
                  -- case compare opref (Just priorityref) of -- unsafeThrowException (error $ "Preemptive CT sibling" <> project log <> "/" <> show op)
                  --   LT -> ref
                  --   GT -> insert opref t ref
                  --   EQ -> unsafeThrowException (error $ "We're trying to insert the same op twice")
                Nothing -> insert opref [t] ref
      newNdxInv = case (insertAt end t ndxinv) of
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
-- | Build an `Op` on the local `Log` appending the `CodePoint` at the end of the weave.
-- |
buildSnocOp :: Log -> CodePoint -> Op
buildSnocOp log@(Log (Timestamp rep i) a b c ndxinv e) val =
  case unsnoc ndxinv of 
    Just {init, last} -> Op (Timestamp rep (i + 1)) (Just last) val
    Nothing -> Op (Timestamp rep (i + 1)) Nothing val

-- |
-- | Build an `Op` at `Replica` which inserts the `CodePoint` after the `Timestamp`,
-- | i.e. the `ref` in the causal tree structure.
-- |
buildCausalOp :: Log -> Timestamp -> CodePoint -> Op
buildCausalOp log@(Log (Timestamp rep i) a b c d e) ref@(Timestamp rep' i') val =
  Op (Timestamp rep (i + 1)) (Just ref) val

buildSnocStringOps :: Log -> String -> Array Op
buildSnocStringOps l s =
  let chars = toCodePointArray s
  in (Array.foldl go {ops: [] :: Array Op, log: l} chars).ops
    where 
      go {ops, log} c = 
        let op = buildSnocOp log c
        in  {ops: snoc ops $ op , log: appendOp log op}

buildCausalStringOps :: Log ->Timestamp -> String -> Array Op
buildCausalStringOps l ref s = maybe [] identity $ do
  { head: ch, tail: chars} <- uncons $ toCodePointArray s
  causalOp <- Just $ buildCausalOp l ref ch
  pure $ (Array.foldl go {ops: [causalOp] :: Array Op, log: appendOp l causalOp} chars).ops
    where 
      go {ops, log} c = 
        let op = buildSnocOp log c
        in  {ops: snoc ops $ op , log: appendOp log op}

-- buildSnocStringOps :: Log -> Replica -> String -> Array Op
-- buildSnocStringOps l r s =
--   let chars = toCodePointArray s
--   in (Array.foldl go {ops: [] :: Array Op, log: l} chars).ops
--     where 
--       go {ops, log} c = 
--         let op = buildSnocOp log r c
--         in  {ops: snoc ops $ op , log: appendOp log op}

-- buildCausalStringOps :: Log -> Replica ->Timestamp -> String -> Array Op
-- buildCausalStringOps l r ref@(Timestamp r i) s = maybe [] identity $ do
--   { head: ch, tail: chars} <- uncons $ toCodePointArray s
--   causalOp <- Just $ buildCausalOp l ref ch
--   pure $ (Array.foldl go {ops: [causalOp] :: Array Op, log: appendOp l causalOp} chars).ops
--     where 
--       go {ops, log} c = 
--         let op = buildSnocOp log r c
--         in  {ops: snoc ops $ op , log: appendOp log op}

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