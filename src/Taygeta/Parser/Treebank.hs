{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}
{-# OPTIONS_GHC -Wall #-}


module Taygeta.Parser.Treebank
    ( treebank
    ) where


import           Data.Bifunctor        (first)
import           Data.Foldable         (foldl')
import qualified Data.Text             as T
import           Data.Text.ICU         (MatchOption (..), Regex, regex)
import           Data.Text.ICU.Replace

import           Taygeta.Parser.Utils
import           Taygeta.Types


treebank :: Tokenizer
treebank = T.words
         . foldRepl (stage2 ++ contractions)
         . flip (joinEnds ' ') ' '
         . foldRepl stage1

reOpts :: [MatchOption]
reOpts = [CaseInsensitive]

foldRepl :: [(T.Text, Replace)] -> T.Text -> T.Text
foldRepl res t = foldl' rall t $ map (first (regex reOpts)) res

rall :: T.Text -> (Regex, Replace) -> T.Text
rall t (re, r) = replaceAll re r t

contractions, stage1, stage2 :: [(T.Text, Replace)]

contractions = map (, " $1 $2 ") [ "\\b(can)(not)\\b"
                                 , "\\b(d)('ye)\\b"
                                 , "\\b(gim)(me)\\b"
                                 , "\\b(gon)(na)\\b"
                                 , "\\b(got)(ta)\\b"
                                 , "\\b(lem)(me)\\b"
                                 , "\\b(mor)('n)\\b"
                                 , "\\b(wan)(na)\\b"
                                 , " ('t)(is)\\b"
                                 , " ('t)(wa)\\b"
                                 , "\\b(whad)(dd)(ya)\\b"
                                 , "\\b(wha)(t)(cha)\\b"
                                 -- Commented out in the original:
                                 , "\\b(whad)(dd)(ya)\\b"
                                 , "\\b(wha)(t)(cha)\\b"
                                 ]

stage1 = [ ("^\"", "``")                  -- starting quotes
         , ("(``)", " $1 ")
         , ("([ (\\[{])\"", "$1 `` ")
         , ("([:,])([^\\d])", " $1 $2 ")  -- punctuation
         , ("\\.\\.\\.", " ... ")
         , ("[;@#$%&]", " $0 ")
         , ("([^\\.])(\\.)([\\]\\)}>\"']*)\\s*$", "$1 $2$3")
         , ("[?!]", " $0 ")
         , ("([^'])' ", "$1 ' ")
         , ("[\\]\\[\\(\\)\\{\\}\\<\\>]", " $0 ")     -- brackets
         , ("--", " -- ")
         ]

stage2 = [ ("\"", " '' ")
         , ("(\\S)('')", "$1 $2 ")
         , ("([^' ])('[sS]|'[mM]|'[dD]|') ", "$1 $2 ")
         , ("([^' ])('ll|'LL|'re|'RE|'ve|'VE|n't|N'T) ", "$1 $2 ")
         ]
