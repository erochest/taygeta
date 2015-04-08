module Taygeta.Types
    ( PlainToken
    , Tokenizer
    , PlainTokenizer
    ) where


import qualified Data.Text as T


type PlainToken     = T.Text
type Tokenizer a    = T.Text -> [a]
type PlainTokenizer = Tokenizer PlainToken

