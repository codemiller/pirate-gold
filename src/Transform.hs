{-# LANGUAGE OverloadedStrings #-}

module Transform (
         toPigLatin
       ) where

import Model.Definition
import Data.Monoid ((<>))
import qualified Data.Text.Lazy as D (Text, words, unwords, length, head, tail, toLower) 

toPigLatin :: [Definition] -> [(Definition, D.Text)]
toPigLatin = foldr (\def acc -> (def, processWords (meaning def)):acc) [] 

processWords :: D.Text -> D.Text
processWords s = D.unwords (foldr (\word acc -> (translate word):acc) [] (D.words s))

translate :: D.Text -> D.Text
translate w
    | D.length w < 2 = w
    -- | otherwise = D.tail w <> (D.toLower (D.head w)) <> "ay"
    | otherwise = D.tail w <> "ay"
