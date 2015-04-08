module Taygeta.Parser.Sexpr
    ( sexpr
    ) where


import           Control.Applicative
import           Data.Attoparsec.Text
import qualified Data.Text            as T

import           Taygeta.Parser.Utils


sexpr :: Parser [T.Text]
sexpr = sepBy word (many1 space)

word :: Parser T.Text
word =   (joinEnds <$> char '(' <*> manym (word <|> anyCharP) <*> char ')')
     <|> takeWhile1 isWord
