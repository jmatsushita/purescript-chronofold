module Benchmark where

import Data.Chronofold (Log, Op, Replica(..), appendOp, appendOps, buildSnocOp, buildSnocStringOps, emptyLog, project, root)
import Data.Lens (preview)
-- import Data.Lens.Record
import Data.Maybe (Maybe, maybe)
import Data.String (CodePoint, codePointFromChar, fromCodePointArray, singleton)
-- import Debug.Trace
import Prelude (class Show, Unit, const, identity, map, show, ($), (<>))
import Test.QuickCheck (mkSeed)
import Test.QuickCheck.Gen (Gen, evalGen, vectorOf)

import Benchotron.Core (Benchmark, benchFn, mkBenchmark)
-- import Benchotron.StdIO (stdoutWrite)
-- import Benchotron.UI.Console (benchmarkToStdout, runBenchM', runBenchmarkConsole, runSuite)
-- import Benchotron.Utils (unsafeJsonStringify)
-- import Data.Array ((..))
import Data.Array as Array
import Data.Char (fromCharCode)
-- import Data.Either (either)
-- import Data.Enum (toEnum)
-- import Data.Foldable (foldMap, foldr)
-- import Data.Int.Bits ((.&.))
import Data.Lens.Index (ix)
-- import Data.Lens.Indexed (itraversed, positions)
-- import Data.Lens.Lens.Tuple (_1)
-- import Data.Monoid.Additive (Additive(..))
-- import Data.Monoid.Multiplicative (Multiplicative(..))
import Data.Newtype (class Newtype, unwrap)
-- import Data.Symbol (SProxy(..))
-- import Data.TraversableWithIndex (traverseWithIndex)
-- import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Console (logShow)
-- import Pipes (each)
import Test.QuickCheck.Arbitrary (class Arbitrary, arbitrary)

newtype CP = CP CodePoint
derive newtype instance showCP :: Show CP
derive instance newtypeCP :: Newtype CP _

instance arbCodePoint :: Arbitrary CP where
  arbitrary = map (\n -> CP $ codePointFromChar $ maybe ('B') identity (fromCharCode n)) (arbitrary :: Gen Int)

arrayOps :: Array CodePoint
arrayOps = map unwrap $ evalGen (arbitrary :: Gen (Array CP)) { newSeed : mkSeed 10 , size : 1000 }

buildBench :: Array CodePoint -> Array Op
buildBench codepoints =
  let
    alpha = Replica 1
    alphaOpRoot = root alpha
    alphaLogRoot = appendOp (emptyLog alpha) alphaOpRoot
    go :: Array Op -> CodePoint -> Array Op
    go ops cp = Array.snoc ops $ buildSnocOp alphaLogRoot cp
  in
    Array.foldl go [] codepoints

-- Error: didn't expect the timestamp to already exist in ndx(Log α2 [(CodePoint 0x0),(CodePoint 0x76CD)] [2,∞] (fromFoldable [(Tuple α1 0),(Tuple α2 1)]) [α1,α2] (fromFoldable [(Tuple Nothing [α1]),(Tuple (Just α1) [α2])]))/(Op α2 (Just α1) (CodePoint 0x13CD))

appendBench :: Array CodePoint -> Log
appendBench codepoints = 
  let
    alpha = Replica 1
    alphaOpRoot = root alpha
    alphaLogRoot = appendOp (emptyLog alpha) alphaOpRoot
    go :: Log -> CodePoint -> Log
    go log cp = appendOp log $ buildSnocOp alphaLogRoot cp
  in
    Array.foldl go alphaLogRoot codepoints

appendStringBench :: Array CodePoint -> Log
appendStringBench cps = 
  let
    alpha = Replica 1
    alphaOpRoot = root alpha
    alphaLogRoot = appendOp (emptyLog alpha) alphaOpRoot
  in
    appendOps alphaLogRoot $ buildSnocStringOps alphaLogRoot $ fromCodePointArray cps

projectBench :: Log -> String
projectBench = project 

type Whatever = { name :: String, number :: Int, either :: Maybe Boolean }

arrayWhatever :: Array Whatever
arrayWhatever = evalGen (vectorOf 1000 arbitrary) { newSeed : mkSeed 10 , size : 1000 }

foldWat :: String
foldWat = Array.foldl (\acc {name, number, either} -> acc <> maybe (show number) (const name) either) "" arrayWhatever 

foldConcatString :: Array CodePoint -> String
foldConcatString = Array.foldl (\acc (cp) -> acc <> singleton cp) ""

benchChronofold :: Benchmark
benchChronofold = mkBenchmark
  { slug: "purescript-chronofold"
  , title: "Simple benchmarks"
  , sizes: [1000] -- 10, 100, 500, 1000] -- (1..5) <#> (_ * 1000)
  , sizeInterpretation: "Number of elements in the array"
  , inputsPerSize: 1
  , gen: \n -> vectorOf n (map unwrap (arbitrary :: Gen CP))
  , functions: [ benchFn "foldArray" foldConcatString
               , benchFn "buildBench" (buildBench)
              --  , benchFn "appendBench" (appendBench)
               ]
  }

-- Unfinished attempt to get only a few interesting numbers (avg, std,...) from Benchotron with lenses
-- However:
--   - benchotron has a lot of moving parts (hard to profile with v8 chrome profiler)
--   - we don't need a generator for each step.
main :: Effect Unit
main = do
  -- logShow $ evalGen (arbitrary :: Gen (Array CP)) { newSeed : mkSeed 10 , size : 10 }
  -- quickCheck (\(CP p) -> const (true) $ spy "CP" (singleton p))
  -- runSuite [benchChronofold]
  -- _ <- pure $ buildOps example 
  -- pure unit
  -- let peering = prop (SProxy :: SProxy "series") 
  --               <<< traversed 
  --               <<< prop (SProxy :: SProxy "results")
  --               <<< traversed 
  --               <<< prop (SProxy :: SProxy "stats")

  -- do
  --   (results) <- runBenchM' $ runBenchmarkConsole benchChronofold
  --   stdoutWrite $ unsafeJsonStringify (over peering identity results)
  let l = ix 1 -- itraversed
  logShow $ preview l [1,2,3] -- {one: 1, two: 2, three : 3}
  -- logShow $ over l (1 + _) {one: 1, two: 2, three : 3}