module Taygeta.Tokenizer
    ( splitTokenizer
    , parserTokenizer
    , sexprTokenizer
    , charTokenizer
    , lineTokenizer
    , treebankTokenizer
    , regexTokenizer
    ) where


import           Control.Applicative
import           Data.Attoparsec.Text
import           Data.Char               (isSpace)
import           Data.Maybe
import qualified Data.Text               as T
import           Data.Text.ICU

import           Taygeta.Parser.Sexpr
import           Taygeta.Parser.Treebank
import           Taygeta.Parser.Utils
import           Taygeta.Types


splitTokenizer :: PlainTokenizer
splitTokenizer = T.split isSpace

parserTokenizer :: Parser T.Text -> PlainTokenizer
parserTokenizer p = parseTokens (many (alt p anyChar) <* takeText)

sexprTokenizer :: PlainTokenizer
sexprTokenizer = parseTokens sexpr

charTokenizer :: Tokenizer Char
charTokenizer = T.unpack

lineTokenizer :: PlainTokenizer
lineTokenizer = T.lines

treebankTokenizer :: PlainTokenizer
treebankTokenizer = treebank

regexTokenizer :: T.Text -> PlainTokenizer
regexTokenizer re = mapMaybe (group 0) . findAll (regex' [UnicodeWord] re)
