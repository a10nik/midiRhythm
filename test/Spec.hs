import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)

import Data.List
import MidiRhythm.Rhythm
import MidiRhythm.NotePress

main :: IO ()
main = hspec $
  describe "MidiRhythm.NotePress" $ do
    describe "splitByTimes" $ do
      it "with one threshold partitions the list" $
        splitByTimes id [5] [1 .. 10] `shouldBe` [[1 .. 4], [5 .. 10]]

      it "with one threshold partitions any sorted integer list" $
        property $ \t xs ->
          let sorted = sort xs
              [before, after] = splitByTimes id [t] sorted
          in (before, after) == partition (< (t :: Int)) sorted

    describe "getSuperimposedChunks" $ do
      it "yields parallel list if there are no chords" $
        getSuperimposedChunks id 1 3 3 3 [1..6]
          `shouldBe`
            [ ( [1], [4] )
            , ( [2], [5] )
            , ( [3], [6] ) ]

      it "groups the chords together" $
        getSuperimposedChunks id 1 3 3 3 ([1, 1, 2] ++ [4, 4, 5])
          `shouldBe`
          [ ( [1, 1], [4, 4] )
          , ( [2], [5] )
          , ( [], [] ) ]

      it "works when downbeat sometimes is not present" $
        getSuperimposedChunks id 1 3 3 3 ([1, 1, 2] ++ [5, 6, 6])
          `shouldBe`
          [ ( [1, 1], [] )
          , ( [2], [5] )
          , ( [], [6, 6] ) ]

      it "scales the bars to the same coordinates" $
        getSuperimposedChunks id 1 3 6 3 ([1, 2, 3] ++ [4, 6, 8])
          `shouldBe`
          [ ( [1], [4] )
          , ( [2], [6] )
          , ( [3], [8] ) ]

    describe "fitness" $ do
      let conf = FitnessConfig (ChordDiffCoeffs 1 2 3 4) 5 6
      it "is 0 for silence in equal bars" $
        getFitness conf [3, 3, 3, 3] [] `shouldBe` 0

      it "is 0 for equal bars" $
        getFitness conf [40, 40]
          [ NotePress 10 100 200 50, NotePress 50 100 200 50] `shouldBe` 0

      it "is non 0 for stretched bars" $
        getFitness conf [40, 80]
          [ NotePress 10 100 200 50, NotePress 60 100 200 50] `shouldNotBe` 0

      it "is more when bars are substantially different" $ do
        let diff1 = getFitness conf [40, 40]
              [ NotePress 10 100 200 50, NotePress 50 105 200 50]
            diff2 = getFitness conf [40, 40]
              [ NotePress 10 100 200 50, NotePress 50 160 200 50]
        diff1 `shouldSatisfy` (< diff2)
