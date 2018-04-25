module Uid
    ( generateUID
    ) where

import Control.Monad

letters :: [String]
letters = [1..] >>= flip replicateM ['a'..'z']

generateUID :: Int -> String
generateUID = (letters !!)