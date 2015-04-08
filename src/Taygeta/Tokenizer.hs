module Taygeta.Tokenizer
    ( splitTokenizer
    , parserTokenizer
    ) where


import           Control.Applicative
import           Data.Attoparsec.Text
import           Data.Char            (isSpace)
import qualified Data.Text            as T

import           Taygeta.Parser.Utils
import           Taygeta.Types


splitTokenizer :: Tokenizer
splitTokenizer = T.split isSpace

parserTokenizer :: Parser T.Text -> Tokenizer
parserTokenizer p = parseTokens (many (alt p anyChar) <* takeText)
