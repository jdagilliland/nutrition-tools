module Meals where

import Control.Applicative

data Macros = Macros { fat :: Float
                     , carbs :: Float
                     , protein :: Float
                     } deriving Show

data MacroData = MacroData { macros :: Macros
                           , volume :: Maybe Float
                           , mass :: Float
                           } deriving Show

scaleByMass :: Float -> MacroData -> MacroData
scaleByMass newMass (MacroData macro portionVolume portionMass) = let
    scale = newMass / portionMass
    newFat = (fat macro) * scale
    newCarbs = (carbs macro) * scale
    newProtein = (protein macro) * scale
    newVolume = fmap (* scale) portionVolume
    in
    MacroData { macros = Macros { fat = newFat
                                , carbs = newCarbs
                                , protein = newProtein
                                }
              , volume = newVolume
              , mass = newMass
              }

combineFoods :: MacroData -> MacroData -> MacroData
combineFoods (MacroData macro1 vol1 mass1) (MacroData macro2 vol2 mass2) = let
    newFat = fat macro1 + fat macro2
    newCarbs = carbs macro1 + carbs macro2
    newProtein = protein macro1 + protein macro2
    combMacro = Macros { fat = newFat
                       , carbs = newCarbs
                       , protein = newProtein
                       }
    newVolume = (+) <$> vol1 <*> vol2 -- revised to account for maybes
    newMass = mass1 + mass2
    in
    MacroData { macros = combMacro
              , volume = newVolume
              , mass = newMass
              }

-- Delete below following early testing phase.
oats = MacroData ( Macros 5 54 10 ) (Just 250) 80
almonds = MacroData (Macros 15 6 6 ) Nothing 30
