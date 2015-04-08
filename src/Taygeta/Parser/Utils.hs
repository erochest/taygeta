
module Taygeta.Parser.Utils
    ( parseTokens
    , alt
    , manym
    , isParen
    , isWord
    , anyCharP
    , joinEnds
    ) where


import           Control.Applicative
import           Data.Attoparsec.Text
import           Data.Char            (isSpace)
import           Data.Foldable        (fold)
import qualified Data.Text            as T

import           Taygeta.Types


parseTokens :: Parser [T.Text] -> PlainTokenizer
parseTokens p = fold . parseOnly (p <* endOfInput)

alt :: Parser a -> Parser b -> Parser a
alt p s = p <|> (s *> alt p s)

manym :: Monoid m => Parser m -> Parser m
manym p = mconcat <$> many p

anyCharP :: Parser T.Text
anyCharP = T.singleton <$> satisfy (not . isParen)

isWord :: Char -> Bool
isWord c | isParen c = False
         | isSpace c = False
         | otherwise = True

isParen :: Char -> Bool
isParen '(' = True
isParen ')' = True
isParen _   = False

joinEnds :: Char -> T.Text -> Char -> T.Text
joinEnds a b c = a `T.cons` b `T.snoc` c
