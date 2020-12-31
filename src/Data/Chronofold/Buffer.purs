module Data.Chronofold.Buffer where


import Data.Array (foldl, (!!))
import Data.Chronofold.Types (Index(..), Log(..))
import Data.Enum (fromEnum)
import Data.Maybe (Maybe(..), maybe)
import Data.String (CodePoint, splitAt)
import Data.String as S
import Data.String.CodePoints (singleton)
import Data.String.CodeUnits (dropRight)
import Prelude (bind, identity, ($), (-), (<>))

-- |
-- | Build an `Op` which inserts the `CodePoint` at the given position.
-- |
-- buildInsertOp :: Log -> Int -> CodePoint -> Op
-- buildInsertOp log@(Log (Timestamp rep i) a b c d e) val =
--   Op (Timestamp rep (i + 1)) (Just (Timestamp rep (i))) val

-- appendString :: Log -> String -> Log
-- appendString l s = 
--   let chars = toCodePointArray s
--   in  foldl (\l' c -> appendOp l' $ buildSnocOp l' c) l chars 

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
    go i acc cps nexts = maybe acc identity $ do
      cp <- cps !! i
      case nexts !! i of
        Just (Index nxt) -> do
          case fromEnum cp of 
            --- skip 0 (root and other 0s)
            0x0000 -> Just $ go nxt acc cps next -- skip 0
            -- 
            0x0008 -> Just $ go nxt (dropRight 1 acc) cps next
            _      -> Just $ go nxt (acc <> singleton cp) cps next
        Just Infinity -> 
          case fromEnum cp of 
            --- skip 0 (root and other 0s)
            0x0000 -> Just $ acc <> singleton cp -- skip 0
            0x0008 -> Just $ dropRight 1 acc
            _      -> Just $ acc <> singleton cp
        Nothing -> Nothing
