module Meals where

import Control.Applicative
import Data.Monoid
import Data.Csv

data Macros = Macros { fat :: Float
                     , carbs :: Float
                     , protein :: Float
                     } deriving Show

data MacroData = MacroData { macros :: Macros
                           , volume :: Maybe Float
                           , mass :: Float
                           } deriving Show

instance Monoid MacroData where
    mempty = MacroData (Macros 0 0 0) (Just 0) 0
    mappend = combineFoods

scaleMacros :: Float -> Macros -> Macros
scaleMacros scale (Macros oldFat oldCarbs oldProtein) =
    Macros (oldFat * scale) (oldCarbs * scale) (oldProtein * scale)

scaleMacroData :: Float -> MacroData -> MacroData
scaleMacroData scale (MacroData oldMacros oldVol oldMass) =
    MacroData { macros = scaleMacros scale oldMacros
              , volume = fmap (* scale) oldVol
              , mass = oldMass * scale
              }

scaleByMass :: Float -> MacroData -> MacroData
scaleByMass newMass macroData = let
    scale = newMass / mass macroData
    in
    scaleMacroData scale macroData

scaleByVol :: Float -> MacroData -> MacroData
scaleByVol vol macroData = undefined
-- scaleByVol vol macroData = let
--     scale = fmap (\x -> vol / x) $ volume macroData
--     in
--     scaleMacroData scale macroData

combineFoods :: MacroData -> MacroData -> MacroData
combineFoods (MacroData macro1 vol1 mass1) (MacroData macro2 vol2 mass2) =
    let
    newFat = fat macro1 + fat macro2
    newCarbs = carbs macro1 + carbs macro2
    newProtein = protein macro1 + protein macro2
    combMacro = Macros { fat = newFat
                       , carbs = newCarbs
                       , protein = newProtein
                       }
    in
    MacroData { macros = combMacro
              , volume = (+) <$> vol1 <*> vol2
              , mass = mass1 + mass2
              }

calories :: Macros -> Float
calories (Macros gFat gCarbs gProtein) =
    9 * gFat + 4 * gCarbs + 4 * gProtein

-- Delete below following early testing phase.
oats = MacroData ( Macros 5 54 10 ) (Just 250) 80
almonds = MacroData (Macros 15 6 6 ) Nothing 30
blackBeans = MacroData ( Macros 1.8 66.5 24.5 ) Nothing 427
riceCooked = MacroData ( Macros 0 45 4 ) (Just 250) 158
bellPepper = MacroData ( Macros 0 10 2 ) Nothing 226
-- mexicanRiceBeans = scaleByMass
