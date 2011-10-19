module Taggable where

data Tag = Tag { name :: String } deriving (Show, Eq)

class Taggable a where
  tags :: a -> [Tag]

tagFactory n = Tag { name=n }


taggedWith e t = filter (\e -> t `elem` tags e) e
taggedWithP e t = length (filter (\e -> t `elem` tags e) e) > 0