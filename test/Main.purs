module Test.Main where

import Data.Array (cons, snoc, unsafeIndex)
import Data.Chronofold (Index(..), Log(..), Op(..), Replica(..), Timestamp(..), appendOp, appendOps, buildCausalOp, buildCausalStringOps, buildSnocOp, buildSnocStringOps, emptyLog, naiveProject, ndxInv, project, root)
import Data.Enum (toEnum)
import Data.Map (fromFoldable)
import Data.Maybe (Maybe(..), fromJust, maybe)
import Data.Monoid (power)
import Data.String (codePointFromChar, singleton)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Aff (Aff, launchAff_)
import Effect.Class (liftEffect)
import Effect.Console (log, logShow)
import Partial.Unsafe (unsafePartial)
import Prelude (class Monad, Unit, bottom, discard, identity, map, pure, unit, ($), (<>), (<$>))
import Test.Spec (SpecT, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner (runSpec)


-- example :: Effect Unit
-- example = do
--   let 
--     -- helper functions for "typing" characters and strings locally
--     -- typeCharLocal :: Log -> Char -> Op
--     -- typeCharLocal l@(Log (Timestamp r i) c next ndxM ndxinv ref) chr = 
--     --   Op (Timestamp r (i + 1)) 
--     -- typeStringLocal :: Log -> String -> Array Op
--     alpha = Replica 1
--     beta = Replica 2
--     -- data Log = Log ReplicaIndex Chronofold Next NextInv Ref 
--     -- data Op = Op Timestamp (Maybe (Timestamp)) Value

--     -- op3alpha = Op (Timestamp alpha 3) (Just (Timestamp alpha 2)) (codePointFromChar 'I')
--     -- alphaLog3 = appendOps (emptyLog alpha) [op2alpha, op3alpha]
--     -- op4alpha = Op (Timestamp alpha 4) (Just (Timestamp alpha 3)) (codePointFromChar 'I')
--     -- alphaLog4 = appendOps (emptyLog alpha) [op2alpha, op3alpha, op4alpha]

--     alphaLogPinsk = appendString (emptyLog alpha) $ "PINSK" <> backspace
--     -- We'll probably want to:
--     --   - convert a keyboard event to an op
--     --   - in parallel:
--     --      - send the op over the wire
--     --      - append the op to the local log
--     --        -> update of the text buffer on screen (project)
--     --   - receive an op on the wire
--     --      - append the op to the local log
--     --        -> update of the text buffer on screen (project)

--     -- betaLogPinsk = appendOp 
  
--     -- logShow (codePointFromChar $ maybe ' ' identity $ fromCharCode 8)
--     backspace = maybe "" identity $ map singleton (toEnum 0x0008)

--   logShow $ "PINSK" <> backspace <> backspace <> "K" 
--   -- render? ByteString

--   logShow $ alphaLogPinsk
--   log $ naiveProject alphaLogPinsk

--   pure unit

tests :: forall m. Monad m => SpecT Aff Unit m Unit
tests = do
  describe "Chronofold" do
  -- it "QuickChecks" $ do
  --   -- liftEffect $ log "Running Chronofold tests"
  --   chronofoldTests
    it "Checks appendOp" $ do
      let
        alpha = Replica 1
        alphaLogRoot = appendOp (emptyLog alpha) $ root alpha

        op2alpha = Op (Timestamp alpha 2) (Just (Timestamp alpha 1)) (codePointFromChar 'P')
        alphaLog2 = appendOp alphaLogRoot op2alpha
      
      alphaLog2 `shouldEqual` 
          (Log 
            (Timestamp alpha 2) 
            [bottom, (codePointFromChar 'P')] 
            [Index 1, Infinity] 
            (fromFoldable 
              [ (Tuple (Timestamp alpha 1) 0)
              , (Tuple (Timestamp alpha 2) 1)])
            [ (Timestamp alpha 1), (Timestamp alpha 2)] 
            (fromFoldable 
              [ (Tuple (Timestamp alpha 1) Nothing)
              , (Tuple (Timestamp alpha 2) (Just (Timestamp alpha 1)))]))

    -- it "Checks appendString & naiveProject" $ do
    --   let 
    --     alpha = Replica 1
    --     backspace = maybe "" identity $ map singleton (toEnum 0x0008)
    --     alphaLogRoot = appendOp (emptyLog alpha) $ root alpha

    --     alphaLogPinsk = appendString alphaLogRoot $ "PINSK" <> backspace

    --   naiveProject alphaLogPinsk `shouldEqual` "PINS"

    it "Checks simple replication" $ do
      let 
        -- Example from https://youtu.be/dKzMZsg5EVA?t=1614
        alpha = Replica 1
        beta = Replica 2

        -- alphaLogRoot
        -- ndx       1α 
        -- val        0  
        -- nxt        ∞     
        -- t         α1 
        -- ref        0
        alphaLogRoot = appendOp (emptyLog alpha) $ root alpha

        -- alphaOpP
        -- t            α2 
        -- ref          α1
        -- val           P
        alphaOpP = buildSnocOp alphaLogRoot (codePointFromChar 'P')

        -- alphaLogP
        -- i          0  1
        -- ndx       1α 2α 
        -- val        0  P 
        -- nxt       2α  ∞   
        -- t         α1 α2
        -- ref        0 α1 
        alphaLogP = appendOp alphaLogRoot alphaOpP

        alphaOpI = buildSnocOp alphaLogP (codePointFromChar 'I')
        alphaLogPI = appendOp alphaLogP alphaOpI

        alphaOpN = buildSnocOp alphaLogPI (codePointFromChar 'N')
        alphaLogPIN = appendOp alphaLogPI alphaOpN

        alphaOpS = buildSnocOp alphaLogPIN (codePointFromChar 'S')
        alphaLogPINS = appendOp alphaLogPIN alphaOpS

        alphaOpK = buildSnocOp alphaLogPINS (codePointFromChar 'K')
        alphaLogPINSK = appendOp alphaLogPINS alphaOpK

        -- Starting with `emptyLog alpha` on beta, means that alpha started 
        -- the document, and we send the root operation from alpha to beta.
        betaLogRoot = appendOp (emptyLog beta) $ root alpha

        -- betaLogP
        -- i          0  1
        -- ndx       1β 2β 
        -- val        0  P 
        -- nxt       2β  ∞   
        -- t         β1 α2
        -- ref        0 α1 
        betaLogP = appendOp betaLogRoot alphaOpP

        betaLogPI = appendOp betaLogP alphaOpI
        betaLogPIN = appendOp betaLogPI alphaOpN
        betaLogPINS = appendOp betaLogPIN alphaOpS
        betaLogPINSK = appendOp betaLogPINS alphaOpK

        backspace = unsafePartial $ fromJust $ toEnum 0x0008
        -- buildCausalOp inserts backspace after the timestamp of P
        Op timestampAlphaOpP _ _ = alphaOpP
        betaOpbackspace = buildCausalOp betaLogPINSK timestampAlphaOpP backspace
        betaLogPINSKbackspace = appendOp betaLogPINSK betaOpbackspace

        betaOpM = buildSnocOp betaLogPINSKbackspace (codePointFromChar 'M')
        betaLogMINSK = appendOp betaLogPINSKbackspace betaOpM

      -- liftEffect $ logShow betaLogPINSKbackspace
      -- liftEffect $ logShow betaLogMINSK
      project betaLogMINSK `shouldEqual` "MINSK"


    it "Checks buildSnocStringOps and buildCausalStringOps" $ do
      let 
        alpha = Replica 1
        beta = Replica 2

        alphaOpRoot = root alpha
        alphaLogRoot = appendOp (emptyLog alpha) alphaOpRoot

        alphaOpsPINSK = buildSnocStringOps alphaLogRoot "PINSK"
        alphaLogPINSK = appendOps alphaLogRoot alphaOpsPINSK

        betaLogRoot = appendOp (emptyLog beta) alphaOpRoot
        betaLogPINSK = appendOps betaLogRoot alphaOpsPINSK

        -- we find the insertion point 
        betaInsertionTimestamp = unsafePartial $ fromJust $ ndxInv betaLogPINSK 1

        backspace = unsafePartial $ fromJust $ toEnum 0x0008
        betaOpsbackspaceM = buildCausalStringOps betaLogPINSK betaInsertionTimestamp $ (singleton backspace) <> "M"

        betaLogMINSK = appendOps betaLogPINSK betaOpsbackspaceM

      project betaLogMINSK `shouldEqual` "MINSK"

    it "Checks replication with 3 replicas and no conflicts" $ do
      let 
        -- Example from https://youtu.be/dKzMZsg5EVA?t=1614
        alpha = Replica 1
        beta = Replica 2
        gamma = Replica 3

        alphaOpRoot = root alpha
        alphaLogRoot = appendOp (emptyLog alpha) alphaOpRoot

        alphaOpsPINSK = buildSnocStringOps alphaLogRoot  "PINSK"
        alphaLogAlpha6 = appendOps alphaLogRoot alphaOpsPINSK

        -- -- Starting with `emptyLog alpha` on beta, means that alpha started 
        -- -- the document, and we send the root operation from alpha to beta.
        betaLogRoot = appendOp (emptyLog beta) alphaOpRoot
        betaLogAlpha6 = appendOps betaLogRoot alphaOpsPINSK

        -- we find the insertion point 
        betaInsertionTimestamp = unsafePartial $ fromJust $ ndxInv betaLogAlpha6 1

        backspace = unsafePartial $ fromJust $ toEnum 0x0008
        betaOpsbackspaceM = buildCausalStringOps betaLogAlpha6 betaInsertionTimestamp $ (singleton backspace) <> "M"

        betaLogAlpha6Beta8 = appendOps betaLogAlpha6 betaOpsbackspaceM

        gammaLogRoot = appendOp (emptyLog gamma) alphaOpRoot
        gammaLogAlpha6 = appendOps gammaLogRoot alphaOpsPINSK

        gammaOpsbackspace4insk = buildSnocStringOps gammaLogAlpha6 $ power (singleton backspace) 4 <> "insk"

        gammaLogAlpha6Gamma14 = appendOps gammaLogAlpha6 gammaOpsbackspace4insk

        gammaLogAlpha6Gamma14Beta8 = appendOps gammaLogAlpha6Gamma14 betaOpsbackspaceM 

        betaLogAlpha6Beta8Gamma14 = appendOps betaLogAlpha6Beta8 gammaOpsbackspace4insk

      project betaLogAlpha6Beta8Gamma14 `shouldEqual` "Minsk"
      project gammaLogAlpha6Gamma14Beta8 `shouldEqual` project betaLogAlpha6Beta8Gamma14
      pure unit

    it "Checks replication with conflicts" $ do
      let 
        -- Example from http://archagon.net/blog/2018/03/24/data-laced-with-history/#causal-trees
        one = Replica 1
        two = Replica 2
        three = Replica 3

        oneOpRoot = root one
        oneLogRoot = appendOp (emptyLog one) oneOpRoot

        oneOpsCMD = buildSnocStringOps oneLogRoot "CMD"
        oneLogCMD = appendOps oneLogRoot oneOpsCMD

        twoLogRoot = appendOp (emptyLog two) oneOpRoot
        twoLogCMD = appendOps twoLogRoot oneOpsCMD

        threeLogRoot = appendOp (emptyLog three) oneOpRoot
        threeLogCMD = appendOps threeLogRoot oneOpsCMD

        twoOpsDEL = buildSnocStringOps twoLogCMD "DEL"
        twoLogCMDDEL = appendOps twoLogCMD twoOpsDEL

        threeOpsALT = buildSnocStringOps threeLogCMD "ALT"
        threeLogCMDALT = appendOps threeLogCMD threeOpsALT

        backspace = unsafePartial $ fromJust $ toEnum 0x0008
        
        oneOpsbackspace2TRL = buildSnocStringOps oneLogCMD $ power (singleton backspace) 2 <> "TRL"
        oneLogCTRL = appendOps oneLogCMD oneOpsbackspace2TRL

        oneLogCTRLALTDEL = appendOps oneLogCTRL (twoOpsDEL <> threeOpsALT)
        twoLogCTRLALTDEL = appendOps twoLogCMDDEL (oneOpsbackspace2TRL <> threeOpsALT)
        threeLogCTRLALTDEL = appendOps threeLogCMDALT (twoOpsDEL <> oneOpsbackspace2TRL)

      project twoLogCMDDEL `shouldEqual` "CMDDEL"
      project threeLogCMDALT `shouldEqual` "CMDALT"
      project oneLogCTRL `shouldEqual` "CTRL"

      liftEffect $ log $ project oneLogCTRLALTDEL       
      liftEffect $ log $ project twoLogCTRLALTDEL       
      liftEffect $ log $ project threeLogCTRLALTDEL       
      project oneLogCTRLALTDEL `shouldEqual` "CTRLALTDEL"
      project twoLogCTRLALTDEL `shouldEqual` "CTRLALTDEL"
      project threeLogCTRLALTDEL `shouldEqual` "CTRLALTDEL"
      pure unit

main :: Effect Unit
main = do
  -- Example
  -- example

  -- node tests
  launchAff_ $ runSpec [consoleReporter] tests
  
------------------------------------------

------------------------------------------
------------------------------------------

-- https://hyp.is/DOqPukSIEeuM5XOYoboTiA/arxiv.org/pdf/2002.09511.pdf

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


-- when an op is shared it's shared with values in timestamps (the shared coordinate system)

-- ndx 1β 2β 3β 4β 5β 6β 7β 8β 
--      0  P  I  N  S  K <=  M <= <= <= <=  i  n  s  k


-- ref α1 α2 α3 α4 α5 α6 β7 β8 γ7 γ8 γ8 γ8 γ8 γ8 γ8 γ8 
--
-- prv6                   1                 
-- prv7         7β        1 α2           
-- prv8         8β        1 α2           

-- nxt6                   1                 
-- nxt7      7β           1 α3                 
-- nxt8      7β           1    α3   
--    
-- ref7                  α6 
-- ref8                  α6 

--        1γ 2γ 3γ 4γ 5γ 6γ 7γ 8γ 9γ Aγ Bγ Cγ Dγ Eγ Fγ Gγ  
--         0  P  I  N  S  K <= <= <= <=  i  n  s  k <=  M 
--        α1 α2 α3 α4 α5 α6 γ7 γ8 γ9 γA γB γC γD γE β7 β8 
--
-- prv6                   1                 
-- prv7         7β        1 α2           
-- prv8         8β        1 α2                      α2 

-- nxt6                 1                 
-- nxt7      7γ           1 α3                 
-- nxt8      7γ           1    α3             
-- nxt9      7γ           1       α3                 
-- nxtA      7γ           1          α3             
-- nxtB      7γ           1             α3             
-- nxtC      7γ           1                α3             
-- nxtD      7γ           1                   α3             
-- nxtE      7γ           1                      α3             
-- nxtF      7γ           1                      α3 α3  
-- 
-- ref                      α6
-- ref                                              α6
-- 

-- α6 : "PINSK" 

-- τ    0   1  2   3 
-- α  :    α6
--        (6α)
--           \
-- β  :       α6  
-- tβ        (6β)     

-- The next_ndx value in the chronofold struct points to the next entry to
-- maintain an append only representation of the weave as a linked list. 
-- We obtain the CT parent from the ref data structure.

--    (e.g. nxt7γ(γ7) = α3    ; nxt⁻¹7γ(γ7) = α2;  
--          nxt8γ(γ7) =  0/γ8 ; nxt⁻¹8γ(γ7) = α2; nxt8γ(γ8) = α3 ; nxt⁻¹8γ(γ8) = 0/γ7 
--    )


-- http://archagon.net/blog/2018/03/24/data-laced-with-history/#causal-trees
--
--          1α 2α 3α 4α 5α 6α 7α 8α 9α Aα 
--           0  C  M  D <= <= T  R  L   D
--          α1 α2 α3 α4 α5 α6 α7 α8 α9 β4 
-- nxt4α              1     
-- nxt5α           |     1
-- nxt6α        |           1
-- nxt7α       !α7             1                  -- after render α2 is followed by α7 but
-- nxt8α                          1               -- we don't care until render
-- nxt9α                             1
-- nxtAα                                1

--       1β 2β 3β 4β 5β 6β 7β  
--        0  C  M  D  D  E  L
--       α1 α2 α3 α4 β5 β6 β8
-- nxt4β           1                                 
-- nxt7β                    1                                 

--       1γ 2γ 3γ 4γ 5γ 6γ 7γ 
--        0  C  M  D  A  L  T
--       α1 α2 α3 γ4 γ5 γ6 γ7
-- nxt4γ           1                                 
-- nxt7β                    1                                 

