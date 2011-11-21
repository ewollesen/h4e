module Taggable where

data Tag = Tag { name :: String } deriving (Show, Ord, Eq)

class Taggable a where
  tags :: a -> [Tag]

tagFactory n = Tag { name=n }


taggedWith e t = length (filter (\e -> t `elem` tags e) e) > 0
isTaggedWith e t = t `elem` (tags e)

hasTag t e = isTaggedWith e t