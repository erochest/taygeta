module Taygeta.Tokenizer
    ( splitTokenizer
    ) where


import qualified Data.Text as T
import Data.Char (isSpace)

import Taygeta.Types


splitTokenizer :: Tokenizer
splitTokenizer = T.split isSpace
