module MemorableName
  ( MemorableName (..),
    memorableName,
    getRandUpTo,
    nouns,
    adjectives,
  )
where

import Control.Monad.Trans (MonadIO, liftIO)
import System.Random (getStdRandom, randomR)

-- | Representation of a memorable name.
data MemorableName
  = MemorableName String String

instance Show MemorableName where
  show (MemorableName a n) = a ++ "-" ++ n

-- | Get a memorable name!
memorableName :: MonadIO m => m MemorableName
memorableName = do
  aIdx <- liftIO $ getRandUpTo (length adjectives)
  nIdx <- liftIO $ getRandUpTo (length nouns)
  return $ MemorableName (adjectives !! aIdx) (nouns !! nIdx)

-- | Get a random integer between 0 and the provided limit, exclusive.
getRandUpTo :: MonadIO m => Int -> m Int
getRandUpTo n
  | n <= 0 = return 0
  | otherwise = liftIO $ getStdRandom (randomR (0, n - 1))

nouns :: [String]
nouns = officeSupplies

adjectives :: [String]
adjectives = animalAdjs

officeSupplies :: [String]
officeSupplies =
  [ "bulletin",
    "cabinet",
    "calculator",
    "chair",
    "computer",
    "desk",
    "eraser",
    "fridge",
    "paper",
    "pen",
    "pencil",
    "phone",
    "plant",
    "printer",
    "router",
    "scanner",
    "sofa",
    "staple",
    "stapler",
    "stationary",
    "table"
  ]

animalAdjs :: [String]
animalAdjs =
  [ "anserine",
    "aquiline",
    "avine",
    "bovine",
    "cameline",
    "canine",
    "cervine",
    "corvine",
    "crocodiline",
    "elephantine",
    "equine",
    "feline",
    "hircine",
    "hominine",
    "leonine",
    "lupine",
    "pavonine",
    "piscine",
    "porcine",
    "serpentine",
    "taurine",
    "tigrine",
    "ursine",
    "vespine",
    "vulpine",
    "zebrine"
  ]
