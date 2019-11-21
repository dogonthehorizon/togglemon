module MemorableName (MemorableName(..), memorableName, getRandUpTo, nouns, adjectives) where

import Control.Monad.Trans (MonadIO, liftIO)
import System.Random       (getStdRandom, randomR)

-- | Representation of a memorable name.
data MemorableName =
  MemorableName String String

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
    | n <= 0    = return 0
    | otherwise = liftIO $ getStdRandom (randomR (0, n - 1))

nouns :: [String]
nouns = officeSupplies

adjectives :: [String]
adjectives = animalAdjs

officeSupplies :: [String]
officeSupplies =
    [ "bulletin"
    , "calculator"
    , "chair"
    , "computer"
    , "desk"
    , "eraser"
    , "fridge"
    , "paper"
    , "pen"
    , "pencil"
    , "plant"
    , "printer"
    , "scanner"
    , "staple"
    , "stapler"
    ]

animalAdjs :: [String]
animalAdjs =
    [ "aquiline"
    , "bovine"
    , "cervine"
    , "corvine"
    , "elephantine"
    , "equine"
    , "hircine"
    , "hominine"
    , "leonine"
    , "lupine"
    , "piscine"
    , "serpentine"
    , "ursine"
    , "vulpine"
    ]
