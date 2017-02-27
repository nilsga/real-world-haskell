module Ch04
 (
  safeHead,
  safeTail,
  safeLast,
  safeInit
 ) where

safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead (head : _) = Just head

safeTail :: [a] -> Maybe [a]
safeTail [] = Nothing
safeTail (_ : tail) = Just tail

safeLast :: [a] -> Maybe a
safeLast [] = Nothing
safeLast list = Just $ last list

safeInit :: [a] -> Maybe [a]
safeInit [] = Nothing
safeInit list = Just $ init list