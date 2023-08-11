module MusicalScale
  ( MusicalScale
  , unMusicalScale
  , mkMusicalScale
  ) where

newtype MusicalScale = MusicalScale [Int]
  deriving (Eq, Show)

unMusicalScale :: MusicalScale -> [Int]
unMusicalScale (MusicalScale ns) = ns

-- | Smart constructor. Allows only ints summing to 12.
mkMusicalScale :: [Int] -> Maybe MusicalScale
mkMusicalScale ns
  | sum ns == 12   = Just (MusicalScale ns)
  | otherwise      = Nothing
