module Test.Data.Chronofold where

import Prelude

import Control.Alt ((<|>))
import Data.Array as A
import Data.Foldable (foldl, for_, all, and)
import Data.FoldableWithIndex (foldrWithIndex)
import Data.Function (on)
import Data.FunctorWithIndex (mapWithIndex)
import Data.List (List(..), groupBy, length, nubBy, singleton, sort, sortBy, (:))
import Data.List.NonEmpty as NEL
import Data.Map as M
import Data.Map.Gen (genMap)
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.NonEmpty ((:|))
import Data.Tuple (Tuple(..), fst, uncurry)
import Effect (Effect)
import Effect.Console (log)
import Partial.Unsafe (unsafePartial)
import Test.QuickCheck ((<?>), (===), quickCheck, quickCheck')
import Test.QuickCheck.Arbitrary (class Arbitrary, arbitrary)
import Test.QuickCheck.Gen (elements, oneOf)

newtype TestMap k v = TestMap (M.Map k v)

instance arbTestMap :: (Eq k, Ord k, Arbitrary k, Arbitrary v) => Arbitrary (TestMap k v) where
  arbitrary = TestMap <$> genMap arbitrary arbitrary

data SmallKey = A | B | C | D | E | F | G | H | I | J
derive instance eqSmallKey :: Eq SmallKey
derive instance ordSmallKey :: Ord SmallKey

instance showSmallKey :: Show SmallKey where
  show A = "A"
  show B = "B"
  show C = "C"
  show D = "D"
  show E = "E"
  show F = "F"
  show G = "G"
  show H = "H"
  show I = "I"
  show J = "J"

instance arbSmallKey :: Arbitrary SmallKey where
  arbitrary = elements $ A :| [B, C, D, E, F, G, H, I, J]

data Instruction k v = Insert k v | Delete k

instance showInstruction :: (Show k, Show v) => Show (Instruction k v) where
  show (Insert k v) = "Insert (" <> show k <> ") (" <> show v <> ")"
  show (Delete k) = "Delete (" <> show k <> ")"

instance arbInstruction :: (Arbitrary k, Arbitrary v) => Arbitrary (Instruction k v) where
  arbitrary = oneOf $ (Insert <$> arbitrary <*> arbitrary) :| [Delete <$> arbitrary]

runInstructions :: forall k v. Ord k => List (Instruction k v) -> M.Map k v -> M.Map k v
runInstructions instrs t0 = foldl step t0 instrs
  where
  step tree (Insert k v) = M.insert k v tree
  step tree (Delete k) = M.delete k tree

smallKey :: SmallKey -> SmallKey
smallKey k = k

number :: Int -> Int
number n = n

smallKeyToNumberMap :: M.Map SmallKey Int -> M.Map SmallKey Int
smallKeyToNumberMap m = m

chronofoldTests :: Effect Unit
chronofoldTests = do

  -- Data.Map

  log "Test inserting into empty tree"
  quickCheck $ \k v -> M.lookup (smallKey k) (M.insert k v M.empty) == Just (number v)
    <?> ("k: " <> show k <> ", v: " <> show v)

  log "Test inserting two values with same key"
  quickCheck $ \k v1 v2 ->
    M.lookup (smallKey k) (M.insert k v2 (M.insert k v1 M.empty)) == Just (number v2)

  log "Test insertWith combining values"
  quickCheck $ \k v1 v2 ->
    M.lookup (smallKey k) (M.insertWith (+) k v2 (M.insert k v1 M.empty)) == Just (number (v1 + v2))

  log "Test insertWith passes the first value as the first argument to the combining function"
  quickCheck $ \k v1 v2 ->
    M.lookup (smallKey k) (M.insertWith const k v2 (M.insert k v1 M.empty)) == Just (number v1)

  log "Test delete after inserting"
  quickCheck $ \k v -> M.isEmpty (M.delete (smallKey k) (M.insert k (number v) M.empty))
    <?> ("k: " <> show k <> ", v: " <> show v)

  log "Test pop after inserting"
  quickCheck $ \k v -> M.pop (smallKey k) (M.insert k (number v) M.empty) == Just (Tuple v M.empty)
    <?> ("k: " <> show k <> ", v: " <> show v)