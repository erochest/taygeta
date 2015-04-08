
module Taygeta.Parser.Utils
    ( parseTokens
    , alt
    ) where


import           Control.Applicative
import           Data.Attoparsec.Text
import           Data.Foldable        (fold)
import qualified Data.Text            as T

import           Taygeta.Types


parseTokens :: Parser [T.Text] -> Tokenizer
parseTokens p = fold . parseOnly (p <* endOfInput)

alt :: Parser a -> Parser b -> Parser a
alt p s = p <|> (s *> alt p s)

