module Ch04
 (
  safeHead,
  safeTail,
  safeLast,
  safeInit,
  splitWith
 ) where

import Debug.Trace

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

splitWith :: (a -> Bool) -> [a] -> [[a]]
splitWith predicate list = splitWith' list []
  where
    splitWith' [] acc = reverse acc
    splitWith' list acc =
      let (part, rest) = break predicate list
      in splitWith' (maybe [] tail (safeTail rest)) (part : acc)
