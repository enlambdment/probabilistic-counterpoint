module StepProbability where

-- line run for Emacs commands
-- char run for shell commands

import qualified Data.List as L
import qualified Data.Map as M
import qualified Control.Monad as CM
import qualified Numeric.Probability.Distribution as D
import qualified Numeric.Probability.Random as R

type Probability = Double
type Dist = D.T Probability

normalOverN :: Int -> Dist Int
normalOverN m = D.normal [1..m]

normal10 :: Dist Int
normal10 = normalOverN 10

-- how to "convolve" 2 normal distributions to get a new one?
-- first, need a way to specify a normal distribution
-- with events spanning a certain range
-- and centered at a certain peak
normalAtPeakWithTailSize :: Int -> Int -> Dist Int
normalAtPeakWithTailSize peak tailSize =
  D.normal [(peak - tailSize)..(peak + tailSize)]

-- e.g.
normalPeak3TailSize5 :: Dist Int
normalPeak3TailSize5 = normalAtPeakWithTailSize 3 5

-- example of a fair vs. unfair die
fairDie :: Dist Int
fairDie = D.uniform [1..6]

unfairDie :: Dist Int
unfairDie = D.fromFreqs [(1,1), (2,4), (3,1), (4,1), (5,1), (6,1)]

-- example of a normal distribution
normal100 :: Dist Int
normal100 = normalOverN 100

{-
How does sampling from distributions work, exactly?

from Numeric.Probability.Random as R:
R.T a ~ State Random.StdGen a
  i.e. R.T is like wrapping a value in a State-ful context
  with the default "the standard pseudo-random number generator
  provided in [random]".
  In particular, `instance RandomGen StdGen`.

When we specify a random variable, we are always working
from Numeric.Probability.Distribution as D:
D.T prob a ~ [(a, prob)]

So, in order to randomly sample from a distribution
(i.e. sample a random variable), we need to travel
  from: D.T prob a ---> to: R.T a

If we have a `dist: D.T prob a` then:
pick :: (Num prob, Ord prob, Random prob) =>
   D.T prob a -> R.T a

then, if we are willing / able to work in the "plain"
monad for side effects, IO:
run :: R.T a -> IO a
-}

samplesGroupedInBins :: Int -> Int -> Dist Int -> IO (M.Map Int Int)
samplesGroupedInBins nSamples binSize dist =
  let samples = CM.replicateM nSamples $
        R.run $ R.pick dist
      sortThenGroup = L.groupBy (\m n -> div m binSize == div n binSize) . L.sort
      mapKeys = L.iterate (+binSize) 1
  in  (M.fromAscList . L.zip mapKeys . (length <$>) . sortThenGroup) <$> samples

{-

Let
  u_j := upper voice's j'th pitch
  l_j := lower voice's j'th pitch

in:

u_0 -> u_1 -> ...

l_0 -> l_1 -> ...

the structure we will evolve by
1. selecting an initial lower pitch, l_0
2. then picking an initial upper pitch, u_0
   that should form a perfect consonance with l_0,
   according to a distribution over the list of consonant pitches.

What follows should be an iterative process:

a. evolve l_j -> l_j+1,
   acc. to a dist' over all available pitches.
   ! In order not to cross voices, we do need to know
     what u_j is, in order to decide what pitches are available.
b. evolve u_j -> u_j+1,
   acc. to a dist' over all pitches *consonant with l_j+1*
c. then proceed.

For the distribution we want to "model" melodic behavior
so we should never leap from v_j -> v_j+1 for either v in {u, l}
in an interval greater than an octave.

Intuitively, we also want to "prefer" steps or small leaps (3rd / 4th)
over larger leaps (> 5th).

How to set that up as a distribution?
For more refined "contouring", consider
working directly with Numeric.Probability.Shape.normalCurve

-}



