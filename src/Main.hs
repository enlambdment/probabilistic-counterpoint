{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}

module Main where

import Euterpea
import Control.Monad
import Control.Monad.IO.Class
import System.Random
import System.Random.Stateful
import qualified Data.List as L
import qualified Numeric.Probability.Distribution as D
import qualified Numeric.Probability.Random as R

aMajorPitchClasses :: [PitchClass]
aMajorPitchClasses = [A, B, Cs, D, E, Fs, Gs]

uniformIdxFromFoldable :: (Foldable t, RandomGen g) =>
                       t a -> g -> (Int, g)
uniformIdxFromFoldable xs = uniformR (0, length xs - 1)

uniformElementFromList :: RandomGen g => [a] -> g -> (a, g)
uniformElementFromList xs g
  = let ng'@(n, g') = uniformIdxFromFoldable xs g in (xs !! n, g')

perfectConsonances :: [Int]
perfectConsonances = [
  0, -- unison
  7  -- perfect fifth
  ]

imperfectConsonances :: [Int]
imperfectConsonances = [
  3, -- minor third
  4, -- major third
  8, -- minor sixth
  9  -- major sixth
  ]
                       
consonances :: [Int]
consonances = perfectConsonances ++ imperfectConsonances

getInterval :: Pitch -> Pitch -> Int
getInterval p1 p2 = abs $ absPitch p1 - absPitch p2

arePitchesConsonant :: Pitch -> Pitch -> Bool
arePitchesConsonant p p' = mod (getInterval p p') 12 `elem` consonances

getConsonantInKeyPitchesAbove :: Pitch -> [Pitch]
getConsonantInKeyPitchesAbove p@(pc, oct) =
  let twoOctavesAbove =
        [ (pc', oct') | oct' <- [oct..(oct+2)],
                        pc'  <- [(Cff)..(Bss)]]
  in filter
     (\q@(pc, oct) ->
        (arePitchesConsonant q p) &&
        (pc `elem` aMajorPitchClasses)
     )
     twoOctavesAbove

harmonizeMelodyAbove :: MonadIO m => [Pitch] -> m [Pitch]
harmonizeMelodyAbove =
  mapM
  (\p ->
     applyAtomicGen (uniformElementFromList $ getConsonantInKeyPitchesAbove p)
                    globalStdGen)

-- new idea using distributions
nextLowerPitchDist :: Pitch -> Pitch -> D.T Double Pitch
nextLowerPitchDist lower@(lpc, loct) upper@(upc, uoct) =
  let availAbsPitches     = [absPitch lower..absPitch upper - 1]
      neighborhoodPitches =
        [ (pc, oct) | oct <- [min loct uoct..max loct uoct],
                      pc  <- aMajorPitchClasses ]
      availPitches        =
        filter (flip elem availAbsPitches . absPitch) neighborhoodPitches
      ordAvailPitches     = L.sortOn absPitch availPitches
  in  D.normal ordAvailPitches
  
nextUpperPitchDist :: Pitch -> Pitch -> D.T Double Pitch
nextUpperPitchDist lower@(lpc, loct) upper@(upc, uoct) =
  let availAbsPitches     = [absPitch lower + 1..absPitch upper + 12]
      neighborhoodPitches =
        [ (pc, oct) | oct <- [min loct uoct..max loct uoct + 2],
                      pc  <- aMajorPitchClasses ]
      availPitches        =
        filter
          (liftM2 (&&) (flip elem availAbsPitches . absPitch)
                       (arePitchesConsonant lower) )
          neighborhoodPitches
      ordAvailPitches     = L.sortOn absPitch availPitches
  in  D.normal ordAvailPitches

-- method to evolve from (l_j, u_j) to (l_j+1, u_j+1)
selectNextPitchPair :: (Pitch, Pitch) -> IO (Pitch, Pitch)
selectNextPitchPair (l, u) = do
  l' <- R.run . R.pick $ nextLowerPitchDist l u
  u' <- R.run . R.pick $ nextUpperPitchDist l' u
  pure (l', u')

pitchPairToMusic :: (Pitch, Pitch) -> Music Pitch
pitchPairToMusic (p, p') =
  note wn p :=: note wn p'

playThenNextPair :: (Pitch, Pitch) -> IO (Pitch, Pitch)
playThenNextPair lu =
  (playDev 2 $ pitchPairToMusic lu) >> (selectNextPitchPair lu)

playIter :: IO (Pitch, Pitch) -> IO (Pitch, Pitch)
playIter iops = do
  ps <- iops
  playDev 2 $ pitchPairToMusic ps
  selectNextPitchPair ps

{-
-- 1. generate basic cantus firmus in MonadIO
mkCantusFirmus :: MonadIO m => m [Pitch]
mkCantusFirmus = ((,4) <$>) <$> replicateM 20
  (applyAtomicGen
   (uniformElementFromList aMajorPitchClasses)
   globalStdGen)

-- 2. then harmonize melody above, also in MonadIO 
mkCounterpoint :: IO ()
mkCounterpoint = do
  cantus <- mkCantusFirmus
  harmony <- harmonizeMelodyAbove cantus
  harmony2 <- harmonizeMelodyAbove cantus
  let cantusMusic = note (wn * 4) <$> cantus
  let harmonyMusic = note (wn * 4) <$> harmony
  -- let harmonyMusic2 = note (hn * 2) <$> harmony2
  let counterpointMusic =
        foldr1 (:+:) cantusMusic :=:
        foldr1 (:+:) harmonyMusic 
        -- :=: foldr1 (:+:) harmonyMusic2
  playDev 2 $ counterpointMusic
-}

{-
Just need to implement now.
Given   x :: a
       mf :: a -> IO a
How to produce infinite list
      mxs :: IO [a] ?
I should use L.iterate.
-}

-- I think I need an `IO (Pitch, Pitch)`
-- that performs the `playDev` musical action,
-- then returns the pitch pair.


main :: IO ()
main =
  let lu0    = return ((A, 2), (A, 4))               :: IO (Pitch, Pitch)
      lus    = L.iterate (>>= playThenNextPair) lu0  :: [IO (Pitch, Pitch)]
  in  (sequence lus) >> return ()
