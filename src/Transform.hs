{-# LANGUAGE OverloadedStrings #-}

module Transform (
         toPigLatin
       ) where

import Model.Definition
import Data.Char (toLower)
import qualified Data.Text.Lazy as D (Text, pack, unpack) 

toPigLatin :: [Definition] -> [(Definition, D.Text)]
toPigLatin = map (\(d, s) -> (d, D.pack s)) . translateMeanings

translateMeanings :: [Definition] -> [(Definition, String)]
translateMeanings = map (\d -> (d, unwords (map translateWord (words (D.unpack (meaning d))))))

translateWord :: String -> String
translateWord (a:b:r) = b:r ++ [(toLower a)] ++ "ay"
translateWord x = x
