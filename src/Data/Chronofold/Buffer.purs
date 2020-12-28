module Data.Chronofold.Buffer where

import Control.MonadZero (guard)
import Data.Array (foldl, (!!))
import Data.Chronofold.Core (Index(..), Log(..), appendOp, buildSnocOp)
import Data.Enum (fromEnum)
import Data.FoldableWithIndex (foldlWithIndex)
import Data.Maybe (Maybe(..), maybe)
import Data.String (CodePoint, splitAt, toCodePointArray)
import Data.String as S
import Data.String.CodePoints (singleton)
import Prelude (($), (-), (<>), (==), discard, bind, pure, identity)

-- |
-- | Build an `Op` which inserts the `CodePoint` at the given position.
-- |
-- buildInsertOp :: Log -> Int -> CodePoint -> Op
-- buildInsertOp log@(Log (Timestamp rep i) a b c d e) val =
--   Op (Timestamp rep (i + 1)) (Just (Timestamp rep (i))) val

appendString :: Log -> String -> Log
appendString l s = 
  let chars = toCodePointArray s
  in  foldl (\l' c -> appendOp l' $ buildSnocOp l' c) l chars 

-- interpret the log into a String.
-- project :: Log -> GPU TypedArray
-- diffable project (incremental). Array Op -> Mutation TypedArray 

naiveProject :: Log -> String
naiveProject (Log _ cps next _ _ _) = foldl go "" cps
  where
    go :: String -> CodePoint -> String
    go b a = case fromEnum a of 
      0x0000 -> b
      0x0008 -> (splitAt ((S.length b) - 1) b).before
      _      -> b <> singleton a

-- Trying to write this with a fold with the relinking
-- doesn't work because we can't skip ahead or back. Seems like
-- a job perfectly suited for a comonad. We'll do that next.
project :: Log -> String
project (Log _ codepoints next _ _ _) = go 0 "" codepoints next
  where
    go :: Int -> String -> Array CodePoint -> Array Index -> String
    go i acc cps ndxs = maybe acc identity $ do
      cp <- cps !! i
      case ndxs !! i of
        Just (Index ndx) -> do
          case fromEnum cp of 
            0x0000 -> Just $ go ndx acc cps next -- skip 0
            0x0008 -> Just $ go ndx (splitAt ((S.length acc) - 1) acc).before cps next
            _      -> Just $ go ndx (acc <> singleton cp) cps next
        Just Infinity -> Just $ acc <> singleton cp
        Nothing -> Nothing
