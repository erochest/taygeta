module Taygeta.Tokenizer
    ( splitTokenizer
    , parserTokenizer
    , sexprTokenizer
    , charTokenizer
    , lineTokenizer
    , treebankTokenizer
    ) where


import           Control.Applicative
import           Data.Attoparsec.Text
import           Data.Char               (isSpace)
import qualified Data.Text               as T

import           Taygeta.Parser.Sexpr
import           Taygeta.Parser.Treebank
import           Taygeta.Parser.Utils
import           Taygeta.Types


splitTokenizer :: Tokenizer
splitTokenizer = T.split isSpace

parserTokenizer :: Parser T.Text -> Tokenizer
parserTokenizer p = parseTokens (many (alt p anyChar) <* takeText)

sexprTokenizer :: Tokenizer
sexprTokenizer = parseTokens sexpr

charTokenizer :: Tokenizer
charTokenizer = map T.singleton . T.unpack

lineTokenizer :: Tokenizer
lineTokenizer = T.lines

treebankTokenizer :: Tokenizer
treebankTokenizer = treebank
