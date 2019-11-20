module MemorableName (MemorableName(), memorableName) where

import Control.Monad.Trans (MonadIO, liftIO)
import System.Random (getStdRandom, randomR)

-- | Representation of a memorable name.
data MemorableName =
  MemorableName String String

instance Show MemorableName where
  show (MemorableName a n) = a ++ "-" ++ n

-- | Get a memorable name!
memorableName :: MonadIO m => m MemorableName
memorableName = do
  aIdx <- liftIO $ getRandInt (length adjectives)
  nIdx <- liftIO $ getRandInt (length nouns)
  return $ MemorableName (adjectives !! aIdx) (nouns !! nIdx)

-- | Get a random integer between 1 and the provided limit.
getRandInt :: MonadIO m => Int -> m Int
getRandInt n = liftIO $ getStdRandom (randomR (0, n - 1))

nouns :: [String]
nouns = animals

animals :: [String]
animals = [
    "bovine",
    "canine",
    "equine",
    "ermine",
    "feline",
    "porcupine"
  ]

adjectives :: [String]
adjectives = [
    "aquiline",
    "cervine",
    "corvine",
    "hircine",
    "leonine",
    "lupine",
    "serpentine"
  ]
