{-# LANGUAGE OverloadedStrings #-}

module Transform (
         toPigLatin
       ) where

import Model.Definition
import Data.Monoid ((<>))
import Data.Char (toLower)
import qualified Data.Text.Lazy as D (Text, words, unwords, length, head, tail, cons) 

toPigLatin :: [Definition] -> [(Definition, D.Text)]
toPigLatin = foldr (\def acc -> (def, processWords (meaning def)):acc) [] 

processWords :: D.Text -> D.Text
processWords s = D.unwords (foldr (\word acc -> (translate word):acc) [] (D.words s))

translate :: D.Text -> D.Text
translate w
    | D.length w < 2 = w
    | otherwise = D.tail w <> ((toLower (D.head w)) `D.cons` "ay")
