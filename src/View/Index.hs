{-# LANGUAGE OverloadedStrings #-}

module View.Index (render) where

import Control.Monad (forM_)
import Model.Definition
import Text.Blaze.Html5.Attributes (href, class_)
import Text.Blaze.Html.Renderer.Text
import Data.Monoid((<>))
import View.Header
import qualified Data.Text.Lazy as D
import qualified Text.Blaze.Html5 as H

-- $setup
-- >>> :set -XOverloadedStrings
-- >>> import Data.List(isPrefixOf, isSuffixOf)
-- >>> import Test.QuickCheck
-- >>> instance Arbitrary Definition where arbitrary = do a <- arbitrary; b <- arbitrary; return (Definition (D.pack a) (D.pack b))
-- >>> instance Arbitrary D.Text where arbitrary = do a <- arbitrary; return (D.pack a)

-- | 
-- 
-- >>> render []
-- "<!DOCTYPE HTML>\n<html><head><title>Pirate Gold</title><link rel=\"stylesheet\" href=\"css/style.css\"></head><body class=\"main\"><div class=\"head\"><h1>Ahoy! Welcome to Pirate Gold.</h1><h2>&#39;ere be some golden terms ye ought to be using me hearties...</h2></div><p>There are no definitions yet.</p><p><a href=\"/add\">Add definition</a></p></body></html>"
--
-- >>> render [(Definition "abc" "def", "efday")]
-- "<!DOCTYPE HTML>\n<html><head><title>Pirate Gold</title><link rel=\"stylesheet\" href=\"css/style.css\"></head><body class=\"main\"><div class=\"head\"><h1>Ahoy! Welcome to Pirate Gold.</h1><h2>&#39;ere be some golden terms ye ought to be using me hearties...</h2></div><ul><li><span class=\"phrase\">ABC</span>: <span class=\"meaning\">def</span> (<span class=\"piglatin\">efday</span>)</li></ul><p><a href=\"/add\">Add definition</a></p></body></html>"
-- >>> render [(Definition "abc" "def", "bcay"), (Definition "abc&def" "ghi&jkl\"mno", "bc&defaay")]
-- "<!DOCTYPE HTML>\n<html><head><title>Pirate Gold</title><link rel=\"stylesheet\" href=\"css/style.css\"></head><body class=\"main\"><div class=\"head\"><h1>Ahoy! Welcome to Pirate Gold.</h1><h2>&#39;ere be some golden terms ye ought to be using me hearties...</h2></div><ul><li><span class=\"phrase\">ABC</span>: <span class=\"meaning\">def</span> (<span class=\"piglatin\">bcay</span>)</li><li><span class=\"phrase\">ABC&amp;DEF</span>: <span class=\"meaning\">ghi&amp;jkl&quot;mno</span> (<span class=\"piglatin\">bc&amp;defaay</span>)</li></ul><p><a href=\"/add\">Add definition</a></p></body></html>"
--
-- prop> let r = D.unpack (render s) in "<!DOCTYPE HTML>\n<html><head><title>Pirate Gold</title><link rel=\"stylesheet\" href=\"css/style.css\"></head><body class=\"main\"><div class=\"head\"><h1>Ahoy! Welcome to Pirate Gold.</h1><h2>&#39;ere be some golden terms ye ought to be using me hearties...</h2></div>" `isPrefixOf` r
--
-- prop> let r = D.unpack (render s) in "<p><a href=\"/add\">Add definition</a></p></body></html>" `isSuffixOf` r
render :: [(Definition, D.Text)] -> D.Text
render definitions = renderHtml . H.docTypeHtml $ do
  header
  H.body H.! class_ "main" $ do
    H.div H.! class_ "head" $ do
      H.h1 "Ahoy! Welcome to Pirate Gold."
      H.h2 "'ere be some golden terms ye ought to be using me hearties..."
    if null definitions 
    then H.p "There are no definitions yet."
    else H.ul . forM_ definitions $ (\def -> H.li $ do 
                                         (H.span H.! class_ "phrase") (H.toHtml (D.toUpper (phrase (fst def)))) <> ": "
                                         (H.span H.! class_ "meaning") (H.toHtml (meaning (fst def))) <> " ("
                                         (H.span H.! class_ "piglatin") (H.toHtml (snd def)) <> ")")
    H.p ((H.a H.! href "/add") "Add definition")
